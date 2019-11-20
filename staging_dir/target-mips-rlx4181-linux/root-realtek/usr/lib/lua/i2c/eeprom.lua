-- Copyright (c) 2014 Frank Edelhaeuser <fedel@users.sourceforge.net>
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to deal
-- in the Software without restriction, including without limitation the rights
-- to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
-- copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
-- OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
-- THE SOFTWARE.
local i2c = require("i2c")


function hexdump(offs, str)
    dump = string.gsub(str, "(.)", function (c)
        if (offs % 16 == 0) then
            prefix = string.format("%04X:  ", offs)
        else
            prefix = ""
        end
        if (offs % 16 == 15) then
            postfix = "\n"
        elseif (offs % 8 == 7) then
            postfix = "  "
        else 
            postfix = " "
        end
        offs = offs + 1
        return prefix..string.format("%02X", string.byte(c))..postfix
    end)
    return dump
end


-- Example: 25AA512 at address 0x50 on bus /dev/i2c-0
local bus = 0
local address = 0x50


-- Read EEPROM at address 0x0000
result, data = i2c.writeread(bus, address, '\0\0', 256)
if (result ~= 0) then
    print("cannot read EEPROM: "..i2c.error(result))
else
    print(hexdump(0, data))
end


-- Write data 0x12 0x34 (decimal: 18 52) to address 0x0078 (decimal: 120)
result = i2c.write(0, 0x50, '\0\120\18\52')
if (result == i2c.SUCCESS) then
    -- Wait until EEPROM write has finished writing
    wait = 10000
    while ((i2c.read(bus, address) ~= i2c.SUCCESS) and (wait > 0)) do
        wait = wait - 1
        if (wait == 0) then
            print("cannot write EEPROM: timeout")
        end
    end
else
    print("cannot write EEPROM: "..i2c.error(result))
end


-- Read EEPROM at address 0x0000
result, data = i2c.writeread(bus, address, '\0\0', 256)
if (result ~= 0) then
    print("cannot read EEPROM: "..i2c.error(result))
else
    print(hexdump(0, data))
end
