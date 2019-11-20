/^# Packages using this file: / {
  s/# Packages using this file://
  ta
  :a
  s/ bison / bison /
  tb
  s/ $/ bison /
  :b
  s/^/# Packages using this file:/
}
