local session = require("luasnip.session")
local util = require("luasnip.util.util")
local node_util = require("luasnip.nodes.util")
local ext_util = require("luasnip.util.ext_opts")
local events = require("luasnip.util.events")
local key_indexer = require("luasnip.nodes.key_indexer")
local types = require("luasnip.util.types")
local opt_args = require("luasnip.nodes.optional_arg")
local snippet_string = require("luasnip.nodes.util.snippet_string")

local Node = {}

function Node:new(o, opts)
	o = o or {}

	-- visible is true if the node is visible on-screen, during normal
	-- expansion, static_visible is needed for eg. get_static_text, where
	-- argnodes in inactive choices will happily provide their static text,
	-- which leads to inaccurate docstrings.
	o.visible = false
	o.static_visible = false
	o.old_text = {}
	o.visited = false
	-- override existing keys, might be necessary due to double-init from
	-- snippetProxy, but shouldn't hurt.
	o = vim.tbl_extend("force", o, node_util.init_node_opts(opts or {}))

	setmetatable(o, self)
	self.__index = self

	return o
end

function Node:get_static_text()
	return self.static_text
end

function Node:get_docstring()
	-- visibility only matters for get_static_text because that's called for
	-- argnodes whereas get_docstring will only be called for actually
	-- visible nodes.
	return self.static_text
end

function Node:put_initial(pos)
	-- access static text directly, get_static_text() won't work due to
	-- static_visible not being set.
	util.put(self.static_text, pos)
	self.visible = true
end

function Node:input_enter(_, _)
	self.visited = true
	self.mark:update_opts(self.ext_opts.active)

	self:event(events.enter)
end

-- dry_run: if not nil, it has to be a table with the key `active` also a table.
-- dry_run.active[node] stores whether the node is "active" in the dry run (we
-- can't change the `active`-state in the actual node, so changes to the
-- active-state are stored in the `dry_run`-table, which is passed to all nodes
-- that participate in the jump)
-- The changes to `active` have to be stored. Otherwise, `dry_run` can lead to
-- endless loops in cases like:
-- ```lua
-- s({ trig = 'n' } , { i(1, "1"), sn(2, {t"asdf"}), i(3, "3") })
-- ```
--
-- Here, jumping from 1 will first set active on the snippetNode, then, since
-- there are no interactive nodes inside it, and since active is set, we will
-- jump to the `i(3)`.
-- If active is not set during the dry_run, we will just keep jumping into the
-- inner textNode.
--
-- A similar problem occurs in nested expansions (insertNode.inner_active
-- is not set).
function Node:jump_into(_, no_move, dry_run)
	if not dry_run then
		self:input_enter(no_move, dry_run)
	end
	return self
end

function Node:jump_from(dir, no_move, dry_run)
	self:input_leave(no_move, dry_run)
	if dir == 1 then
		if self.next then
			return self.next:jump_into(dir, no_move, dry_run)
		else
			return nil
		end
	else
		if self.prev then
			return self.prev:jump_into(dir, no_move, dry_run)
		else
			return nil
		end
	end
end

function Node:jumpable(dir)
	if dir == 1 then
		return self.next ~= nil
	else
		return self.prev ~= nil
	end
end

function Node:get_text()
	if not self.visible then
		return nil
	end
	local ok, text = pcall(function()
		local from_pos, to_pos = self.mark:pos_begin_end_raw()

		-- end-exclusive indexing.
		local lines =
			vim.api.nvim_buf_get_lines(0, from_pos[1], to_pos[1] + 1, false)

		if #lines == 1 then
			lines[1] = string.sub(lines[1], from_pos[2] + 1, to_pos[2])
		else
			lines[1] = string.sub(lines[1], from_pos[2] + 1, #lines[1])

			-- node-range is end-exclusive.
			lines[#lines] = string.sub(lines[#lines], 1, to_pos[2])
		end
		return lines
	end)
	-- if deleted.
	return ok and text or { "" }
end

function Node:get_snippetstring()
	-- if this is not overridden, get_text returns a multiline string.
	return snippet_string.new(self:get_text())
end

function Node:get_static_snippetstring()
	-- if this is not overridden, get_static_text() is a multiline string.
	return snippet_string.new(self:get_static_text())
end

function Node:set_old_text()
	self.old_text = self:get_text()
end

function Node:exit()
	self.visible = false
	self.mark:clear()
end

function Node:get_passive_ext_opts()
	if self.visited then
		return self.ext_opts.visited
	else
		return self.ext_opts.unvisited
	end
end

function Node:input_leave(_, dry_run)
	if dry_run then
		return
	end
	self:event(events.leave)

	self.mark:update_opts(self:get_passive_ext_opts())
end
function Node:input_leave_children() end
function Node:input_enter_children() end

function Node:update() end

function Node:update_static() end

function Node:expand_tabs(tabwidth, indentstrlen)
	util.expand_tabs(self.static_text, tabwidth, indentstrlen)
end

function Node:indent(indentstr)
	util.indent(self.static_text, indentstr)
end

function Node:subsnip_init() end

function Node:init_positions(position_so_far)
	self.absolute_position = vim.deepcopy(position_so_far)
end

function Node:init_insert_positions(position_so_far)
	self.absolute_insert_position = vim.deepcopy(position_so_far)
end

function Node:event(event)
	local node_callback = self.node_callbacks[event]
	if node_callback then
		node_callback(self)
	end

	-- try to get the callback from the parent.
	if self.pos then
		-- node needs position to get callback (nodes may not have position if
		-- defined in a choiceNode, ie. c(1, {
		--	i(nil, {"works!"})
		-- }))
		-- works just fine.
		local parent_callback = self.parent.callbacks[self.pos][event]
		if parent_callback then
			parent_callback(self)
		end
	end

	session.event_node = self
	vim.api.nvim_exec_autocmds("User", {
		pattern = "Luasnip" .. events.to_string(self.type, event),
		modeline = false,
	})
end

local function get_args(node, get_text_func_name, static)
	local argnodes_text = {}
	for key, arg in ipairs(node.args_absolute) do
		local is_optional = opt_args.is_opt(arg)
		if is_optional then
			arg = arg.ref
		end

		local argnode
		if key_indexer.is_key(arg) then
			argnode = node.parent.snippet.dependents_dict:get({
				"key",
				arg.key,
				"node",
			})
		else
			-- since arg may be a node, it may not be initialized in the snippet
			-- and therefore not have an absolute_insert_position. Check for that.
			if not arg.absolute_insert_position then
				-- the node is not (yet, maybe) visible.
				return nil
			end
			local dict_key = arg.absolute_insert_position
			-- will append to arg.absolute_insert_position, but it's restored
			-- two lines down.
			-- (dict:get shouldn't (yeah yeah, you never know, but this really
			-- shouldn't) fail, so we don't worry with pcall)
			table.insert(dict_key, "node")
			argnode = node.parent.snippet.dependents_dict:get(dict_key)
			dict_key[#dict_key] = nil
		end
		-- maybe the node is part of a dynamicNode and not yet generated.
		-- also handle the argnode as not-present if
		-- * we are doing a regular update and it is not visible, or
		-- * we are doing a static update and it is not static_visible or
		--   visible (this second condition is to allow the docstring-generation
		--   to be improved by data provided after the expansion)
		if argnode and ((static and (argnode.static_visible or argnode.visible)) or (not static and argnode.visible)) then
			if not static then
				-- Don't store (aka call get_snippetstring) if this is a static
				-- update (there will be no associated buffer-region!) and
				-- don't store if the node is not visible. (Then there's
				-- nothing to store anyway)

				-- now, store traverses the whole tree, and if one argnode includes
				-- another we'd duplicate some work.
				-- But I don't think there's a really good reason for doing
				-- something like this (we already have all the data by capturing
				-- the outer argnode), and even if it happens, it should occur only
				-- rarely.
				argnode:store()
			end
			local argnode_text = argnode[get_text_func_name](argnode)
			-- can only occur with `get_text`. If one returns nil, the argnode
			-- isn't visible or some other error occured. Either way, return nil
			-- to signify that not all argnodes are available.
			if not argnode_text then
				return nil
			end
			argnodes_text[key] = argnode_text
		else
			if is_optional then
				argnodes_text[key] = nil
			else
				return nil
			end
		end
	end

	return argnodes_text
end

function Node:get_args()
	return get_args(self, "get_snippetstring", false)
end
function Node:get_static_args()
	return get_args(self, "get_static_snippetstring", true)
end

function Node:get_jump_index()
	return self.pos
end

function Node:set_ext_opts(name)
	-- differentiate, either visited or unvisited needs to be set.
	if name == "passive" then
		self.mark:update_opts(self:get_passive_ext_opts())
	else
		self.mark:update_opts(self.ext_opts[name])
	end
end

-- default impl. for textNode and functionNode (fNode stores after an update).
function Node:store() end

function Node:update_restore() end

-- find_node only needs to check children, self is checked by the parent.
function Node:find_node()
	return nil
end

Node.ext_gravities_active = { false, true }

function Node:insert_to_node_absolute(position)
	-- this node is a leaf, just return its position
	return self.absolute_position
end

function Node:set_dependents() end

function Node:set_argnodes(dict)
	if self.absolute_insert_position then
		-- append+remove "node" from absolute_insert_position to quickly create
		-- key for dict.
		table.insert(self.absolute_insert_position, "node")
		dict:set(self.absolute_insert_position, self)
		self.absolute_insert_position[#self.absolute_insert_position] = nil
	end
	if self.key then
		dict:set({ "key", self.key, "node" }, self)
	end
end

function Node:make_args_absolute() end

function Node:resolve_position(position)
	error(
		string.format(
			"invalid resolve_position(%d) on node at %s",
			position,
			vim.inspect(self.absolute_position)
		)
	)
end

function Node:static_init()
	self.static_visible = true
end

-- resolve_*node*_ext_opts because snippet(Node)s have child_ext_opts, which
-- also have to be resolved.
-- This function generates a nodes ext_opts (those actually used in highlighting).
function Node:resolve_node_ext_opts(base_prio, parent_ext_opts)
	if self.merge_node_ext_opts then
		self.ext_opts = ext_util.extend(
			vim.deepcopy(self.node_ext_opts),
			parent_ext_opts or self.parent.effective_child_ext_opts[self.type]
		)
	else
		self.ext_opts = self.node_ext_opts
	end

	ext_util.set_abs_prio(
		self.ext_opts,
		(base_prio or self.parent.ext_opts.base_prio)
			+ session.config.ext_prio_increase
	)
end

function Node:is_interactive()
	-- safe default.
	return true
end

-- initialize active-setting in dry_run-table for `self`.
function Node:init_dry_run_active(dry_run)
	if dry_run and dry_run.active[self] == nil then
		dry_run.active[self] = self.active
	end
end
-- determine whether this node is currently active.
-- This is its own function (and not just a flat table-check) since we have to
-- check the data in the dry_run-table or the node, depending on `dry_run`.
function Node:is_active(dry_run)
	return (not dry_run and self.active) or (dry_run and dry_run.active[self])
end

function Node:get_buf_position(opts)
	opts = opts or {}
	local raw = opts.raw ~= nil and opts.raw or true

	if raw then
		return self.mark:pos_begin_end_raw()
	else
		return self.mark:pos_begin_end()
	end
end

-- only does something for insert- and snippetNode.
function Node:set_sibling_rgravs(_, _, _, _) end

-- when an insertNode receives text, its mark/region should contain all the
-- text that is inserted.
-- This can be achieved by setting the left and right "right-gravity"(rgrav) of
-- the mark, which are responsible for controlling the direction an endpoint of
-- the mark is moved when text is inserted.
-- When a regular insertNode is focused/entered, we would like the left and
-- right rgrav to be false and true, respectively. Example:
--        this is an insertNodeAnd this is another insertNode
-- mark1: l                    r
-- mark2:                      l                     r
-- if `this is an insertNode` should be focused, we have to set the rgrav of
-- l1 false (because inserting text at the column of l1 should not shift l1 to
-- the right). Similarly, the rgrav of r1 has to be set true, text inserted at
-- its column SHOULD move it to the right.
-- Complicating this whole thing: if like above there is an adjacent
-- insertNode, its gravities have to be adjusted as well (if they are not, the
-- insertNodes regions would overlap, which is obviously confusing). So, when
-- adjusting some nodes rgravs, those of the siblings may have to be adjusted as well.
-- Another example:
--        aacc
-- mark1: l r
-- mark2:   l
--          r
-- mark3:   l r
-- (the insertNode for mark2 is not visible at all, l2 and r2 are in the same
-- column)
-- This example highlights that not only the immediate sibling might need
-- adjusting, but all siblings that share a mark-boundary with the node that
-- should be focused.
-- Even further complicating the matter: Snippets are trees, and failing to
-- set the rgrav of snippet adjacent to (sharing an endpoint with) the node we
-- want to focus, regardless of its position in the tree, will lead to extmarks
-- covering the wrong regions.
--
-- More complications: focusing a node does not always mean setting the rgravs
-- such that text will end up inside the node!
-- For example, in the case of a terminating i(0) (like s("trig", {i(1,
-- "text"), t"  ", i(0)})), we would like to NOT include the text entered into
-- it in the snippet. Thus, the gravities of it and all its parents have to be
-- set (in this case) false,false, if the i(0) were at the beginning of the
-- snippet (weird??) they'd have to be true,true.
--
--
-- Unfortunately, we cannot guarantee that two extmarks on the same position
-- also have the same gravities, for exmample if the text inside a focused node
-- is deleted, and then another unrelated node is focused, the two endpoints of
-- the previously focused node will have opposing rgravs.
-- Maybe this whole procedure could be sped up further if we can assume that
-- identical endpoints imply identical rgravs.
local function focus_node(self, lrgrav, rrgrav)
	-- find nodes on path from self to root.
	local nodes_path = node_util.root_path(self)

	-- direction is the direction away from this node, towards the outside of
	-- the tree-representation of the snippet.
	-- This is dubbed "direction" because it is the direction we will search in
	-- to find nodes on one endpoint of self.
	for _, direction in ipairs({ -1, 1 }) do
		local self_direction_endpoint = self.mark:get_endpoint(direction)
		local direction_rgrav = util.ternary(direction == -1, lrgrav, rrgrav)
		local effective_direction_rgrav = direction_rgrav

		-- adjust left rgrav of all nodes on path upwards to root/snippet:
		-- (i st. self and the snippet are both handled)
		for i = 1, #nodes_path do
			local node = nodes_path[i]
			local node_direction_endpoint = node.mark:get_endpoint(direction)

			if
				not util.pos_equal(
					node_direction_endpoint,
					self_direction_endpoint
				)
			then
				-- stop adjusting rgravs once self no longer is on the boundary of
				-- its parents, or if the rgrav is already set correctly.
				break
			end

			node.mark:set_rgrav(direction, effective_direction_rgrav)

			-- Once self's snippet is reached on the root-path, we will only
			-- adjust nodes self should be completely contained inside.
			-- Since the rgravs, however, may be set up otherwise (for example
			-- when focusing on an $0 that is the last node of the snippet), we
			-- have to adjust them now.
			if node.snippet == node then
				effective_direction_rgrav = direction == 1
			end

			-- can't use node.parent, since that might skip nodes (in the case of
			-- dynamicNode, for example, the generated snippets parent is not the
			-- dynamicNode, but its parent).
			-- also: don't need to check for nil, because the
			local node_above = nodes_path[i + 1]
			if node_above then
				node_above:set_sibling_rgravs(
					node,
					self_direction_endpoint,
					direction,
					effective_direction_rgrav
				)
			end
		end
		self:subtree_set_pos_rgrav(
			self_direction_endpoint,
			-direction,
			direction_rgrav
		)
	end
end

function Node:subtree_set_rgrav(rgrav)
	self.mark:set_rgravs(rgrav, rgrav)
end

function Node:subtree_set_pos_rgrav(_, direction, rgrav)
	self.mark:set_rgrav(-direction, rgrav)
end

function Node:focus()
	focus_node(self, false, true)
end

function Node:set_text(text)
	local text_indented = util.indent(text, self.parent.indentstr)

	if self:get_snippet().___static_expanded then
		self.static_text = text_indented
		self:update_dependents_static({ own = true, parents = true })
	else
		if self.visible then
			self:set_text_raw(text_indented)
			self:update_dependents({ own = true, parents = true })
		end
	end
end

function Node:set_text_raw(text)
	self:focus()

	local node_from, node_to = self.mark:pos_begin_end_raw()
	local ok = pcall(
		vim.api.nvim_buf_set_text,
		0,
		node_from[1],
		node_from[2],
		node_to[1],
		node_to[2],
		text
	)
	-- we can assume that (part of) the snippet was deleted; remove it from
	-- the jumplist.
	if not ok then
		error("[LuaSnip Failed]: " .. vim.inspect(text))
	end
end

-- since parents validate the adjacency, nodes where we don't know anything
-- about the text inside them just have to assume they haven't been deleted :D
function Node:extmarks_valid()
	return true
end

function Node:linkable()
	-- linkable if insert or exitNode.
	return vim.tbl_contains(
		{ types.insertNode, types.exitNode },
		self.type
	)
end
function Node:interactive()
	-- interactive if immediately inside choiceNode.
	return vim.tbl_contains(
		{ types.insertNode, types.exitNode },
		self.type
	) or self.choice ~= nil
end
function Node:leaf()
	return vim.tbl_contains(
		{ types.textNode, types.functionNode, types.insertNode, types.exitNode },
		self.type
	)
end

function Node:parent_of(node)
	for i = 1, #self.absolute_position do
		if self.absolute_position[i] ~= node.absolute_position[i] then
			return false
		end
	end

	return true
end

-- self has to be visible/in the buffer.
-- none of the node's ancestors may contain self.
function Node:update_dependents(which)
	-- false: don't set static
	local dependents = node_util.collect_dependents(self, which, false)
	for _, node in ipairs(dependents) do
		if node.visible then
			node:update_restore()
		end
	end
end

function Node:update_dependents_static(which)
	-- true: set static
	local dependents = node_util.collect_dependents(self, which, true)
	for _, node in ipairs(dependents) do
		if node.static_visible then
			node:update_static()
		end
	end
end

function Node:subtree_do(opts)
	opts.pre(self)
	opts.post(self)
end

function Node:get_snippet()
	return self.parent.snippet
end

-- all nodes that can be entered have an override, only need to nop this for
-- those that don't.
function Node:subtree_leave_entered() end

return {
	Node = Node,
	focus_node = focus_node,
}
