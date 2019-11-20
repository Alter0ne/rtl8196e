/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ patch / patch /
  tb
  s/ $/ patch /
  :b
  s/^/# Packages using this file:/
}
