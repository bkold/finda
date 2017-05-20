with GNAT.Regexp;

package Finder is

	type Search_Mode is (
		Plain,
		Regular_Expression);

	procedure Find_Start (
		Directory, Token : in String;
		Desired_Depth : in Natural := Natural'Last);

private

	Max_Depth : Natural;

	Match_Token : GNAT.Regexp.Regexp;

	procedure Find (
		Directory : in String;
		Depth : in Natural);

	procedure Write (Directory : in String);

	task type Thread (Num : Integer) is
		entry Run (D : in String; Depth : in Natural);
	end Thread;

	type Thread_Access is access all Thread;
	type Bool_Array is array (1 .. 3) of Boolean;

	Thread_1 : Thread(1);
	Thread_2 : Thread(2);
	Thread_3 : Thread(3);

	protected Task_Pool_Status is
		procedure Check (Value : out Integer);
		procedure End_Thread (Thread_Num : in Integer);
	private
		Status : Bool_Array := (others=>True);
	end Task_Pool_Status;

end Finder;