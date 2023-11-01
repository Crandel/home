local mp = require 'mp' -- isn't actually required, mp still gonna be defined
local utils = require 'mp.utils'
local assdraw = require 'mp.assdraw'

-- NOTE: should not be altered here, edit options in corresponding .conf file
local opts = {
  -- options for this script --------------------------------------------------
  strip_cmd_at = 65,
  sort_commands_by = 'priority',

  -- options for extended menu ------------------------------------------------
  toggle_menu_binding = 't',
  lines_to_show = 17,
  pause_on_open = true,
  resume_on_exit = "only-if-was-paused", -- another possible value is true

  -- styles
  font_size = 21,
  -- cursor 'width', useful to change if you have hidpi monitor
  cursor_x_border = 0.3,
  line_bottom_margin = 1,
  -- TODO: how to put it in JSON in .conf file?
  -- text_color=[ "default":"000000","accent":"d8a07b","current":"aaaaaa","comment":"ffffff" ]
  -- didn't work
  text_color = {
    default = 'ffffff',
    accent = 'd8a07b',
    current = 'aaaaaa',
    comment = '636363',
  },
  menu_x_padding = 5,
  menu_y_padding = 2,

  search_heading = 'M-x',
  filter_by_fields = { 'cmd', 'key', 'comment' },
  column_layout = false,
}

(require 'mp.options').read_options(opts, mp.get_script_name())

package.path =
    mp.command_native({ "expand-path", "~~/script-modules/?.lua;" }) .. package.path
local em = require "extended-menu"

local mx_menu = em:new(opts)

local data = { list = {} }

function mx_menu:submit(val)
  mp.msg.info(val.cmd)
  mp.command(val.cmd)
end

local function sort_cmd_list()
  table.sort(data.list, function(i, j)
    if opts.sort_commands_by == 'priority' then
      return tonumber(i.priority) > tonumber(j.priority)
    end
    -- sort by command name by default
    return i.cmd < j.cmd
  end)
end

local function get_cmd_list()
  local bindings = mp.get_property_native("input-bindings")

  -- sets a flag 'shadowed' to all binding that have a binding with higher
  -- priority using same key binding
  for _, v in ipairs(bindings) do
    for _, v1 in ipairs(bindings) do
      if v.key == v1.key and v.priority < v1.priority then
        v.shadowed = true
        break
      end
    end
  end

  data.list = bindings

  sort_cmd_list()
end

local function merge_leader_bindings(le, leader_key)
  -- REVIEW: sadly mpvs 'input-bindings' is read only and i can't force set
  -- priority -1 for bindings that are overwritten by leader bindings.
  -- Since leader script needs keybinding to be defined in 'input-bindings'.
  -- Therefore i just merge those leader bindings in my own 'data.list'.

  local bindings_to_append = {}
  local not_found_leader_kbds = {}

  local function split_with_spaces(str)
    local result_str = ''
    for char in str:gmatch '.' do result_str = result_str .. char .. ' ' end
    return result_str:gsub("(.-)%s*$", "%1") -- strip spaces
  end

  for i, lb in ipairs(le) do
    -- overwriting binding in data.list
    for y, b in ipairs(data.list) do
      if b.cmd:find(lb.cmd, 1, true) then
        data.list[y].priority = 13
        data.list[y].key = leader_key .. ' ' .. split_with_spaces(lb.key)
        -- if it's a script binding - initially it won't have comment field
        -- but leader binding can (and should) have comment field, so we set it
        -- and if it is normal keybinding and it had it's own comment field then
        -- leave it as it was
        data.list[y].comment = lb.comment or data.list[y].comment
        goto continue1
      end

      -- if binding was not found - append it to list
      if y == #data.list then
        local binding = {}

        binding.priority = 13
        binding.key = leader_key .. ' ' .. split_with_spaces(lb.key)
        binding.cmd = lb.cmd
        -- if it's a script binding - initially it won't have comment field
        -- but leader binding can (and should) have comment field, so we set it
        -- and if it is normal keybinding and it had it's own comment field then
        -- leave it as it was
        binding.comment = lb.comment

        table.insert(bindings_to_append, binding)
      end
    end
    ::continue1::
  end

  for i, v in ipairs(bindings_to_append) do table.insert(data.list, v) end

  sort_cmd_list()

  -- TODO: handle warning about not found leader kbd better
  -- for i,v in ipairs(not_found_leader_kbds) do
  --   print(v, 'not found')
  -- end
end

-- [i]ndex [v]alue
function em:get_line(_, v)
  local a = assdraw.ass_new()
  -- 20 is just a hardcoded value, cuz i don't think any keybinding string
  -- length might exceed this value
  local comment_offset = opts.strip_cmd_at + 20

  local cmd = v.cmd

  if #cmd > opts.strip_cmd_at then
    cmd = string.sub(cmd, 1, opts.strip_cmd_at - 3) .. '...'
  end

  -- we need to count length of strings without escaping chars, so we
  -- calculate it before defining excaped strings
  local cmdkbd_len = #(cmd .. v.key) + 3 -- 3 is ' ()'

  cmd = self:ass_escape(cmd)
  local key = self:ass_escape(v.key)
  -- 'comment' field might be nil
  local comment = self:ass_escape(v.comment or '')

  local function get_spaces(num)
    -- returns num-length string full of spaces
    local s = ''
    for _ = 1, num do s = s .. '\\h' end
    return s
  end

  -- handle inactive keybindings
  if v.shadowed or v.priority == -1 then
    local why_inactive = (v.priority == -1)
        and 'inactive keybinding'
        or 'that binding is currently shadowed by another one'

    a:append(self:get_font_color('comment'))
    a:append(cmd)
    a:append('\\h(' .. key .. ')')

    if opts.column_layout then
      a:append(get_spaces(comment_offset - cmdkbd_len))
    else
      a:append(' ')
    end

    a:append('(' .. why_inactive .. ')')
    return a.text
  end

  a:append(self:get_font_color('default'))
  a:append(cmd)
  a:append(self:get_font_color('accent'))
  a:append('\\h(' .. key .. ')')
  a:append(self:get_font_color('comment'))

  if opts.column_layout then
    a:append(get_spaces(comment_offset - cmdkbd_len))
  else
    a:append(' ')
  end

  a:append(comment and comment or '')
  return a.text
end

local function update_bindings()
  get_cmd_list()
  mp.commandv("script-message-to", "leader", "leader-bindings-request")
  em:update_list(data.list)
end

-- mp.register_event("file-loaded", get_cmd_list)
get_cmd_list()

-- and register them in script itself
mp.register_script_message("merge-leader-bindings", function(bindings, leader_key)
  bindings = utils.parse_json(bindings)
  merge_leader_bindings(bindings, leader_key)
end)

mp.observe_property('input-bindings', 'native', update_bindings)

-- keybind to launch menu
mp.add_key_binding(opts.toggle_menu_binding, "M-x", function()
  mx_menu:init(data)
end)
