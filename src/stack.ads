generic
	Size : in Positive;
	type Element_Type is private;
package Stack is
	pragma SPARK_Mode (On);
	type Stack_Type is limited private with
		Default_Initial_Condition => Is_Empty (Stack_Type);

	function Is_Empty (Object : in Stack_Type) return Boolean;

	function Is_Full (Object : in Stack_Type) return Boolean;

	procedure Push (Object : in out Stack_Type; Item : in Element_Type) with
		Pre => (not Is_Full(Object));

	procedure Pop (Object : in out Stack_Type; Item : out Element_Type) with
		Pre => (not Is_Empty(Object));

	function Pop (Object : in out Stack_Type) return Element_Type with
		Pre => (not Is_Empty(Object));

private

	type Data_Array is array (1 .. Size) of Element_Type;
	subtype Index_Range is Natural range 0 .. Size;

	protected type Stack_Type is
		function Is_Empty return Boolean;
		function Is_Full return Boolean;
		procedure Push (Item : in Element_Type);
		procedure Pop (Item : out Element_Type);
	private
		S : Positive := Size;
		I : Index_Range := 0;
		Data : Data_Array;
	end Stack_Type;

end Stack;