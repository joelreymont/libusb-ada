with System;
with System.Address_Image;
with Ada.Text_IO;
use Ada.Text_IO;
with USB.Low;
use USB.Low;
with Interfaces.C;
use Interfaces.C;

procedure Enumerate is
   Ctx : System.Address;
   Err : int;
begin
   Err := Init (Ctx);
   Put_Line ("Err: " & Err'Image & ", Context: " & System.Address_Image(Ctx));
   Deinit (Ctx);
end Enumerate;
