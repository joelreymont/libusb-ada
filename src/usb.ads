pragma Ada_2022;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with System;
with GNATCOLL.Refcount;
with Ada.Containers;
use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Strings.Text_Buffers;
with Ada.Iterator_Interfaces;
with Interfaces.C;
with LibUSB;

package USB is

  function Hex (n : in Integer) return String;

  Input_Output_Error, Invalid_Parameter_Error, Access_Denied_Error,
  No_Device_Error, Not_Found_Error, Resource_Busy_Error, Timeout_Error,
  Overflow_Error, Pipe_Error, Syscall_Interrupted_Error, Out_Of_Memory_Error,
  Operation_Not_Supported_Error, Other_Error : exception;

  procedure Init;

  type Context is tagged private;

  function Make_Context return Context;

  type Device is tagged private;

  type Device_List is tagged private with
   Default_Iterator  => Device_List_Iterate, Iterator_Element => Device,
   Constant_Indexing => Device_List_Reference;

  function Get_Device_List (Ctx : Context'Class) return Device_List;

  type Device_List_Cursor is private;

  function Device_List_Has_Element
   (Position : Device_List_Cursor) return Boolean;

  type Device_List_Constant_Reference
   (Element : not null access constant Device) is
  null record with
   Implicit_Dereference => Element;

  function Device_List_Reference
   (Container : aliased in Device_List;
    Position  : Device_List_Cursor)
    return Device_List_Constant_Reference;

  package Device_List_Iterators is new Ada.Iterator_Interfaces
   (Device_List_Cursor, Device_List_Has_Element);

  function Device_List_Iterate
   (Container : in Device_List)
    return Device_List_Iterators.Forward_Iterator'Class;

  type Descriptor_Kind is
   (Device_Descriptor_Kind,
    Config_Descriptor_Kind,
    String_Descriptor_Kind,
    Interface_Descriptor_Kind,
    Endpoint_Descriptor_Kind,
    BOS_Descriptor_Kind,
    Capability_Descriptor_Kind,
    HID_Descriptor_Kind,
    Report_Descriptor_Kind,
    Physical_Descriptor_Kind,
    HUB_Descriptor_Kind,
    SuperSpeed_HUB_Descriptor_Kind,
    SS_Endpoint_Companion_Descriptor_Kind);
  for Descriptor_Kind use
   (Device_Descriptor_Kind                => 16#01#,
    Config_Descriptor_Kind                => 16#02#,
    String_Descriptor_Kind                => 16#03#,
    Interface_Descriptor_Kind             => 16#04#,
    Endpoint_Descriptor_Kind              => 16#05#,
    BOS_Descriptor_Kind                   => 16#0F#,
    Capability_Descriptor_Kind            => 16#10#,
    HID_Descriptor_Kind                   => 16#21#,
    Report_Descriptor_Kind                => 16#22#,
    Physical_Descriptor_Kind              => 16#23#,
    HUB_Descriptor_Kind                   => 16#29#,
    SuperSpeed_HUB_Descriptor_Kind        => 16#2A#,
    SS_Endpoint_Companion_Descriptor_Kind => 16#30#);
  for Descriptor_Kind'Size use Interfaces.C.unsigned_char'Size;

  type Class is
   (Per_Interface_Class,
    Audio_Class,
    Communications_Class,
    HID_Class,
    Physical_Class,
    Image_Class,
    Printer_Class,
    Mass_Storage_Class,
    HUB_Class,
    Data_Class,
    Smart_Card_Class,
    Content_Security_Class,
    Video_Class,
    Personal_Healthcare_Class,
    Diagnostic_Device_Class,
    Wireless_Class,
    Miscellaneous_Class,
    Application_Class,
    Vendor_Specific_Class);
  for Class use
   (Per_Interface_Class       => 16#00#,
    Audio_Class               => 16#01#,
    Communications_Class      => 16#02#,
    HID_Class                 => 16#03#,
    Physical_Class            => 16#05#,
    Image_Class               => 16#06#,
    Printer_Class             => 16#07#,
    Mass_Storage_Class        => 16#08#,
    HUB_Class                 => 16#09#,
    Data_Class                => 16#0A#,
    Smart_Card_Class          => 16#0B#,
    Content_Security_Class    => 16#0D#,
    Video_Class               => 16#0E#,
    Personal_Healthcare_Class => 16#0F#,
    Diagnostic_Device_Class   => 16#0DC#,
    Wireless_Class            => 16#E0#,
    Miscellaneous_Class       => 16#EF#,
    Application_Class         => 16#FE#,
    Vendor_Specific_Class     => 16#FF#);
  for Class'Size use Interfaces.C.unsigned_char'Size;

  type Device_Descriptor is record
    Length                   : Natural;
    Kind                     : Descriptor_Kind;
    Release_Number           : Natural;
    Device_Class             : Class;
    Device_Subclass          : Natural;
    Device_Protocol          : Natural;
    Max_Packet_Size_0        : Natural;
    Vendor_ID                : Natural;
    Product_ID               : Natural;
    Device_Release_Number    : Natural;
    Manufacturer_Index       : Natural;
    Product_Index            : Natural;
    Serial_Number_Index      : Natural;
    Number_Of_Configurations : Natural;
  end record;

  function Get_Device_Descriptor (Dev : Device) return Device_Descriptor;

private

  procedure Check_Error (Error_Code : Interfaces.C.int);

  type Context_Refcounted is new GNATCOLL.Refcount.Refcounted with record
    Context : LibUSB.Context;
  end record with
   Put_Image => Context_Put_Image;

  procedure Context_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Context_Refcounted);

  procedure Context_Release (Self : in out Context_Refcounted);

  package Context_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Context_Refcounted,
    Release      => Context_Release);

  type Context is new Context_Pointers.Ref with null record;

  type Device_Refcounted is new GNATCOLL.Refcount.Refcounted with record
    Device : LibUSB.Device;
  end record with
   Put_Image => Device_Put_Image;

  procedure Device_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Device_Refcounted);

  procedure Device_Release (Self : in out Device_Refcounted);

  package Device_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Device_Refcounted,
    Release      => Device_Release);

  type Device is new Device_Pointers.Ref with null record;

  package Device_Vectors is new Ada.Containers.Vectors
   (Index_Type   => Natural,
    Element_Type => Device);

  type Device_List_Refcounted is new GNATCOLL.Refcount.Refcounted with record
    Start   : LibUSB.Device_List;
    Devices : aliased Device_Vectors.Vector;
  end record with
   Put_Image => Device_List_Put_Image;

  procedure Device_List_Put_Image
   (Output : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
    Value  : Device_List_Refcounted);

  procedure Device_List_Release (Self : in out Device_List_Refcounted);

  package Device_List_Pointers is new GNATCOLL.Refcount.Shared_Pointers
   (Element_Type => Device_List_Refcounted,
    Release      => Device_List_Release);

  type Device_List is new Device_List_Pointers.Ref with null record;

  type Device_List_Cursor is record
    Data : Device_Vectors.Cursor;
  end record;

end USB;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
