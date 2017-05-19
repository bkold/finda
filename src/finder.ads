with GNAT.Regexp;

package Finder is

	type Search_Mode is (
		Plain,
		Regular_Expression);

	procedure Find_Start 
		(Directory, Token : in String; 
		 Desired_Depth : in Natural := Natural'Last);

private

	
	Max_Depth : Natural;

	Match_Token : GNAT.Regexp.Regexp;

	procedure Find (
		Directory : in String; 
		Depth : in Natural);

	procedure Write (Directory : in String);

end Finder;