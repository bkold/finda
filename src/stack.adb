package body Stack is
	pragma SPARK_Mode (On);

	function Is_Empty (Object : in Stack_Type) return Boolean is
	begin
		return Object.Is_Empty;
	end Is_Empty;

	function Is_Full (Object : in Stack_Type) return Boolean is
	begin
		return Object.Is_Full;
	end Is_Full;

	procedure Push (Object : in out Stack_Type; Item : in Element_Type) is
	begin
		Object.Push(Item);
	end Push;

	procedure Pop (Object : in out Stack_Type; Item : out Element_Type) is
	begin
		Object.Pop(Item);
	end Pop;

	function Pop (Object : in out Stack_Type) return Element_Type is
		X : Element_Type;
	begin
		Object.Pop(X);
		return X;
	end Pop;

	protected body Stack_Type is
		function Is_Empty return Boolean is
		begin
			return I = 0;
		end Is_Empty;

		function Is_Full return Boolean is
		begin
			return I = Size;
		end Is_Full;

		procedure Push (Item : in Element_Type) is
		begin
			I := I + 1;
			Data(I) := Item;
		end Push;

		procedure Pop (Item : out Element_Type) is
		begin
			Item := Data(I);
			I := I - 1;
		end Pop;
	end Stack_Type;

end Stack;