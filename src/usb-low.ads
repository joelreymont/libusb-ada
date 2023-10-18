pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with Interfaces.C;
use Interfaces.C;
with Interfaces.C.Strings;
with System;

package USB.Low is

  function Init (ctx : out System.Address) return int with
   Import => True, Convention => C, External_Name => "libusb_init";

  procedure Deinit (ctx : System.Address) with
   Import => True, Convention => C, External_Name => "libusb_exit";

end USB.Low;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
