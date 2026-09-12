local colors = require("colors")
local settings = require("settings")
local center_popup = require("center_popup")

sbar.exec("killall system_stats >/dev/null 2>&1; " .. os.getenv("CONFIG_DIR") .. "/helpers/system_stats/bin/system_stats system_stats_update 3.0")

local cpu_gpu_width = 44
local mem_width = 28
local trailing_gap = 16

local function make_graph(name, icon_text, width, padding_right)
  return sbar.add("graph", name, width, {
    position = "right",
    graph = { color = colors.red },
    icon = {
      string = icon_text,
      color = colors.green,
      font = {
        family = settings.font.text,
        style = settings.font.style_map["Heavy"],
        size = 9.0,
      },
      padding_right = 4,
    },
    label = {
      string = "--",
      color = colors.white,
      font = {
        family = settings.font.numbers,
        style = settings.font.style_map["Bold"],
        size = 9.0,
      },
      align = "right",
      padding_left = 2,
      padding_right = 6,
      width = 0,
      y_offset = 4,
    },
    padding_left = 0,
    padding_right = padding_right or 0,
  })
end

local mem = make_graph("widgets.sys.mem", "MEM", mem_width, trailing_gap)
local gpu = make_graph("widgets.sys.gpu", "GPU", cpu_gpu_width, 0)
local cpu = make_graph("widgets.sys.cpu", "CPU", cpu_gpu_width, 0)

-- Popup setup
local popup_width = 360
local stats_popup = center_popup.create("system_stats.popup", {
  width = popup_width,
  height = 620,
  popup_height = 26,
  title = "System Stats",
  meta = "",
  auto_hide = false,
})
stats_popup.meta_item:set({ drawing = false })
stats_popup.body_item:set({ drawing = false })

local popup_pos = stats_popup.position
local name_width = 300
local value_width = popup_width - name_width
-- 自动计算截断长度：字体大小11，每个字符约7像素
local max_name_chars = math.floor(name_width / 7)

-- Helper to add info rows
local function add_row(key, title)
  return sbar.add("item", "system_stats.popup." .. key, {
    position = popup_pos,
    width = popup_width,
    icon = {
      align = "left",
      string = title,
      width = name_width,
      font = { family = settings.font.text, style = settings.font.style_map["Regular"], size = 11.0 },
    },
    label = {
      align = "right",
      string = "-",
      width = value_width,
      font = { family = settings.font.numbers, style = settings.font.style_map["Regular"], size = 11.0 },
    },
    background = { drawing = false },
  })
end

-- CPU section
stats_popup.add_section("cpu", "CPU")
local cpu_rows = {}
for i = 1, 10 do
  cpu_rows[i] = add_row("cpu_proc" .. i, "")
end

-- MEM section
stats_popup.add_section("mem", "MEM")
local mem_rows = {}
for i = 1, 10 do
  mem_rows[i] = add_row("mem_proc" .. i, "")
end

stats_popup.add_close_row({ label = "close x" })

local function read_text(s)
  return tostring(s or ""):gsub("^%s+", ""):gsub("%s+$", "")
end

local function clip_name(name)
  if #name > max_name_chars then
    return name:sub(1, max_name_chars - 3) .. "..."
  end
  return name
end

-- Cache top-N process snapshots delivered with every system_stats_update event.
-- The helper enumerates processes via public mach APIs (proc_listallpids +
-- proc_pid_rusage) and ships the top 10 entries for both CPU and MEM as
-- key=value args ("top_cpu_1=name|pct", "top_mem_1=name|123 MB", ...).
-- Popup rendering reads from this cache, so opening the popup involves zero
-- shell exec or fork.
local cached_cpu = {}
local cached_mem = {}

local function update_proc_cache(env)
  for i = 1, 10 do
    cached_cpu[i] = env["top_cpu_" .. i]
    cached_mem[i] = env["top_mem_" .. i]
  end
end

-- Render cached process lists into popup rows. Wrapped in begin_config so the
-- ~20 row mutations collapse to a single CA transaction per refresh.
local function render_popup_rows()
  sbar.begin_config()
  for i = 1, 10 do
    local entry = cached_cpu[i]
    if entry and entry ~= "" then
      local name, pct = entry:match("^(.-)|([%d%.]+)$")
      if name and pct then
        cpu_rows[i]:set({
          icon = { string = clip_name(name) },
          label = { string = pct .. "%" },
        })
      else
        cpu_rows[i]:set({ icon = { string = "" }, label = { string = "" } })
      end
    else
      cpu_rows[i]:set({ icon = { string = "" }, label = { string = "" } })
    end

    local mem_entry = cached_mem[i]
    if mem_entry and mem_entry ~= "" then
      local name, val = mem_entry:match("^(.-)|(.+)$")
      if name and val then
        mem_rows[i]:set({
          icon = { string = clip_name(name) },
          label = { string = val },
        })
      else
        mem_rows[i]:set({ icon = { string = "" }, label = { string = "" } })
      end
    else
      mem_rows[i]:set({ icon = { string = "" }, label = { string = "" } })
    end
  end
  sbar.end_config()
end

local function refresh_popup()
  render_popup_rows()
end

-- Click on header title to refresh
stats_popup.title_item:subscribe("mouse.clicked", function(env)
  if env.BUTTON == "left" then
    refresh_popup()
  end
end)

-- Toggle popup
local function toggle_popup()
  if stats_popup.is_showing() then
    stats_popup.hide()
  else
    stats_popup.show(function()
      refresh_popup()
    end)
  end
end

-- All widgets open the same popup
cpu:subscribe("mouse.clicked", function(env)
  if env.BUTTON == "left" then toggle_popup() end
end)

gpu:subscribe("mouse.clicked", function(env)
  if env.BUTTON == "left" then toggle_popup() end
end)

mem:subscribe("mouse.clicked", function(env)
  if env.BUTTON == "left" then toggle_popup() end
end)

cpu:subscribe("system_stats_update", function(env)
  -- Stash latest top-N process snapshot regardless of suspend state so users
  -- opening the popup right after a Mission Control transition still see
  -- fresh data from the latest helper tick.
  update_proc_cache(env)
  if _G.SKETCHYBAR_SUSPENDED then return end

  local cpu_total = tonumber(env.cpu_total)
  local cpu_temp_val = tonumber(env.cpu_temp)
  local cpu_label = cpu_total and string.format("%d%%", cpu_total) or "--"

  if cpu_temp_val and cpu_temp_val >= 0 then
    cpu_label = string.format("%s %dC", cpu_label, cpu_temp_val)
  else
    cpu_label = string.format("%s --C", cpu_label)
  end

  local gpu_util = tonumber(env.gpu_util)
  local gpu_temp_val = tonumber(env.gpu_temp)
  local gpu_label = gpu_util and string.format("%d%%", gpu_util) or "--"

  if gpu_temp_val and gpu_temp_val >= 0 then
    gpu_label = string.format("%s %dC", gpu_label, gpu_temp_val)
  else
    gpu_label = string.format("%s --C", gpu_label)
  end

  local mem_percent = tonumber(env.mem_used_percent)
  local mem_label = (mem_percent and mem_percent >= 0)
      and string.format("%d%%", mem_percent)
      or "--"

  -- Batch all bar mutations into a single message to sketchybar so the
  -- WindowServer only commits one CA transaction per update tick.
  sbar.begin_config()
  if cpu_total then cpu:push({ cpu_total / 100.0 }) end
  if gpu_util and gpu_util >= 0 then gpu:push({ gpu_util / 100.0 }) end
  if mem_percent and mem_percent >= 0 then mem:push({ mem_percent / 100.0 }) end
  cpu:set({ label = cpu_label })
  gpu:set({ label = gpu_label })
  mem:set({ label = mem_label })
  sbar.end_config()

  -- If the popup is open, push the freshly-cached process lists immediately
  -- so users see live updates every tick without reopening the popup.
  if stats_popup.is_showing() then
    render_popup_rows()
  end
end)
