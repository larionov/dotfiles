local settings = require("settings")

local icons = {
  sf_symbols = {
    plus = "􀅼",
    loading = "􀖇",
    apple = "􀣺",
    gear = "􀍟",
    calendar = "􀧞",
    cpu = "􀫥",
    clipboard = "􀉄",
    adapter = "",

    -- App launchers and utilities
    lm_studio = "􀞏",
    onepassword = "􀎦",
    quantumultx = "􀋞",
    synergy = "􀈳",
    time_machine = "􀐫",
    warp = "☁",
    lock = "􀎡",
    translate = "🔍",
    ubuntu = "ⓤ",
    wechat = "􀌤",

    switch = {
      on = "􁏮",
      off = "􁏯",
    },
    volume = {
      _100="􀊩",
      _66="􀊧",
      _33="􀊥",
      _10="􀊡",
      _0="􀊣",
    },
    battery = {
      _100 = "􀛨",
      _75 = "􀺸",
      _50 = "􀺶",
      _25 = "􀛩",
      _0 = "􀛪",
      charging = "􀢋"
    },
    wifi = {
      upload = "􀄨",
      download = "􀄩",
      connected = "􀙇",
      disconnected = "􀙈",
      router = "􁓤",
    },
    media = {
      back = "􀊊",
      forward = "􀊌",
      play_pause = "􀊈",
    },
    refresh = "", -- arrow.triangle.2.circlepath
  },

  -- Alternative NerdFont icons
  nerdfont = {
    plus = "",
    loading = "",
    apple = "",
    gear = "",
    calendar = "󰃭",
    cpu = "",
    clipboard = "", -- fa-paste (clipboard) in Nerd Font
    adapter = "", -- fa-plug

    -- App launchers and utilities
    lm_studio = "", -- code
    onepassword = "", -- key
    quantumultx = "", -- globe/network
    synergy = "", -- link
    time_machine = "", -- history
    warp = "", -- cloud
    lock = "",
    translate = "", -- fa-language
    ubuntu = "",
    github = "", -- fa-github
    wechat = "", -- fa-weixin (WeChat)

    switch = {
      on = "󱨥",
      off = "󱨦",
    },
    volume = {
      _100="",
      _66="",
      _33="",
      _10="",
      _0="",
    },
    battery = {
      _100 = "",
      _75 = "",
      _50 = "",
      _25 = "",
      _0 = "",
      charging = ""
    },
    wifi = {
      upload = "",
      download = "",
      connected = "󰖩",
      disconnected = "󰖪",
      router = ""
    },
    media = {
      back = "",
      forward = "",
      play_pause = "",
    },
    refresh = "", -- nf fallback
  },
}

if not (settings.icons == "NerdFont") then
  return icons.sf_symbols
else
  return icons.nerdfont
end
