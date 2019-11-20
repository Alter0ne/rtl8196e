Introduction
============

quilt-el minor mode (lib/quilt.el) is toggled by `M-x quilt-mode'. It
provides the following features:

- You can issue many quilt sub-commands by shortcut keys. By using
  these keys, quilt-el can also detect moving the patch stack and
  can refresh corresponding buffers automatically.

- If a buffer is associated with the file which is not included in
  the topmost patch, the buffer becomes read-only. So you can avoid
  editing wrong files accidentally.

- After quilt-el is once enabled, all files which you open are checked
  whether it is in a quilt hierarchy or not. If so, corresponding buffer
  becomes quilt-mode automatically.

 -- Satoru Takeuchi <nqm08501@nifty.com>, Sun, 14 Jan 2007 23:57:29 +0900
