local colors = require("colors")

-- Equivalent to the --bar domain
sbar.bar({
  height = 32,
  -- topmost=off so the native menu bar (on hover) and system notifications draw
  -- ABOVE the bar. yabai external_bar reserves the strip, so windows won't cover it.
  topmost = false,
  -- Visual effects (blur + translucency)
  color = colors.bar.bg,
  blur_radius = 20,
  padding_right = 2,
  padding_left = 2,
})
