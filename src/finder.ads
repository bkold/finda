with GNAT.Regexp;

package Finder is

	type Search_Mode is (
		Plain,
		Regular_Expression);

	procedure Find_Start 
		(Directory, Token : in String; 
		 Desired_Mode : in Search_Mode;
		 Desired_Depth : in Natural := Natural'Last);

	Dir_Error : Exception;

private

	Depth : Natural := 0;
	Max_Depth : Natural;

	Run_Mode : Search_Mode := Plain;
	Search_Pattern : GNAT.Regexp.Regexp;
	procedure Find (Directory, Token : in String);
	procedure Write (Directory : in String);


end Finder;