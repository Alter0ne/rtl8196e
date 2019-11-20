/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ findutils / findutils /
  tb
  s/ $/ findutils /
  :b
  s/^/# Packages using this file:/
}
