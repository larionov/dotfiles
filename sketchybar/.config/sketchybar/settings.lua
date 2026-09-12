return {
  paddings = 3,
  icon_paddings = 2,
  group_paddings = 5,

  icons = "NerdFont", -- available: NerdFont, sf-symbols

  -- Shortcuts (right-side compact icon chunk)
  shortcuts_icon_size = 15.0,

  app_icons = {
    enabled = true,
    font = {
      family = "sketchybar-app-font",
      style = "Regular",
      size = 14.0,
    },
    padding_right = 4,
  },

  -- All three roles fall back to Nerd Fonts installed via Homebrew casks so
  -- the bar renders without Apple's restricted SF family or a separately-installed
  -- Sarasa Term SC.
  font = {
    text = "JetBrainsMono Nerd Font", -- Used for text
    numbers = "JetBrainsMono Nerd Font Mono", -- Used for numbers
    icons = "Symbols Nerd Font", -- Used for icons (NerdFont glyphs)
    style_map = {
      ["Regular"] = "Regular",
      ["Semibold"] = "SemiBold",
      ["Bold"] = "Bold",
      ["Heavy"] = "ExtraBold",
      ["Black"] = "ExtraBold",
    },
  },
}
