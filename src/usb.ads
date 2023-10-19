pragma Ada_2012;

pragma Style_Checks (Off);
pragma Warnings (Off, "-gnatwu");

with System;
with GNATCOLL.Refcount;

package USB is

    Input_Output_Error, Invalid_Parameter_Error, Access_Denied_Error,
    No_Device_Error, Not_Found_Error, Resource_Busy_Error, Timeout_Error,
    Overflow_Error, Pipe_Error, Syscall_Interrupted_Error, Out_Of_Memory_Error,
    Operation_Not_Supported_Error, Other_Error : exception;

    type Context is tagged private;

    function Make_Context return Context;

private

    type Context_Contents is new GNATCOLL.Refcount.Refcounted with record
        Address : System.Address;
    end record;

    procedure Context_Release (Self : in out Context_Contents);

    package Context_Pointers is new GNATCOLL.Refcount.Shared_Pointers
       (Element_Type => Context_Contents,
        Release      => Context_Release);

    type Context is new Context_Pointers.Ref with null record;

    --    No_Context : constant Context :

    --       (Context_Pointers.Null_Ref with null record);

end USB;

pragma Style_Checks (On);
pragma Warnings (On, "-gnatwu");
