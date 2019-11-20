/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ m4 / m4 /
  tb
  s/ $/ m4 /
  :b
  s/^/# Packages using this file:/
}
