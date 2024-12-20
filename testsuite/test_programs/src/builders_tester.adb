--
--  Copyright (C) 2024, AdaCore
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--

with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

with GNATCOLL.Traces;

with Prettier_Ada.Documents;          use Prettier_Ada.Documents;
with Prettier_Ada.Documents.Builders; use Prettier_Ada.Documents.Builders;
with Prettier_Ada.Documents.Json;     use Prettier_Ada.Documents.Json;

--  Test program to build a Document_Type using the
--  Prettier_Ada.Documents.Builders package, serialize it into JSON and formats
--  it.

procedure Builders_Tester is

   procedure Test_Align;
   --  Builds a Document_Type using the Align builder

   procedure Test_Alignment_Table;
   --  Builds multiple Document_Type objects using the Alignment_Table builder

   procedure Test_Alignment_Table_Separator;
   --  Builds multiple Document_Type objects using the
   --  Alignment_Table_Separator builder

   procedure Test_Break_Parent;
   --  Builds a Document_Type using the Break_Parent builder

   procedure Test_Continuation_Line_Indent;
   --  Builds a Document_Type using the Continuation_Line_Indent builder

   procedure Test_Cursor;
   --  Builds a Document_Type using the Cursor builder

   procedure Test_Fill;
   --  Builds a Document_Type using the Fill builder

   procedure Test_Group;
   --  Builds a Document_Type using the Group builder

   procedure Test_Conditional_Group;
   --  Builds a Document_Type using the Conditional_Group builder

   procedure Test_Hard_Line;
   --  Builds a Document_Type using the Hard_Line builder

   procedure Test_Hard_Line_Without_Break_Parent;
   --  Builds a Document_Type using the Hard_Line_Without_Break_Parent builder

   procedure Test_If_Break;
   --  Builds a Document_Type using the If_Break builder

   procedure Test_Indent;
   --  Builds a Document_Type using the Indent builder

   procedure Test_Indent_If_Break;
   --  Builds a Document_Type using the Indent_If_Break builder

   procedure Test_Join;
   --  Builds a Document_Type using the Join builder

   procedure Test_Label;
   --  Builds a Document_Type using the Label builder

   procedure Test_Line_Suffix;
   --  Builds a Document_Type using the Line_Suffix builder

   procedure Test_Line_Suffix_Boundary;
   --  Builds a Document_Type using the Line_Suffix_Boundary builder

   procedure Test_List;
   --  Builds a Document_Type using the List builder

   procedure Test_Literal_Line;
   --  Builds a Document_Type using the Literal_Line builder

   procedure Test_Literal_Line_Without_Break_Parent;
   --  Builds a Document_Type using the
   --  Literal_Line_Without_Break_Parent builder.

   procedure Test_No_Document;
   --  Creates documents that contain No_Document and formats them

   procedure Test_String_Literal;
   --  Builds a Document_Type using the String_Literal aspect

   procedure Test_Text;
   --  Builds a Document_Type using the Text builder

   procedure Test_Trim;
   --  Builds a Document_Type using the Trim builder

   -----------------
   --  Test_Align --
   -----------------

   procedure Test_Align is
      Document_1 : constant Document_Type :=
        Align ((Width, 2), Fill (List (["A", Line, "B"])));
      Document_2 : constant Document_Type :=
        Align ((Width, 2), Fill (["A", Line, "B"]));
      Document_3 : constant Document_Type :=
        Indent
          (List
             (["begin",
               Hard_Line,
               "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               "(",
               Align
                 ((Kind => Inner_Root),
                  Fill
                    (["BBBBBBBBBBBBBBBBBBBB,",
                      Line,
                      "CCCCCCCCCCCCCCCCCCCC"])),
               ")"]));

   begin
      Put_Line ("=== Align ===");
      Put_Line ("> Align Document 1 JSON:");
      Put_Line (Serialize (Document_1));
      Put_Line ("> Align Document 1 Formatted:");
      Put_Line (Format (Document_1));
      Put_Line ("> Align Document 2 JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> Align Document 2 Formatted:");
      Put_Line (Format (Document_2));
      Put_Line ("> Align Document 3 JSON:");
      Put_Line (Serialize (Document_3));
      Put_Line ("> Align Document 3 Formatted:");
      Put_Line (Format (Document_3));
      Put_Line ("> Align Document 3 Formatted with Tabs:");
      Put_Line
        (Format
           (Document_3,
            (Width       => 79,
             Indentation =>
               (Kind         => Tabs,
                Width        => 3,
                Continuation => 2,
                Offset       => (Spaces => 0, Tabs => 0)),
             End_Of_Line => LF)));
      New_Line;
   end Test_Align;

   --------------------------
   -- Test_Alignment_Table --
   --------------------------

   procedure Test_Alignment_Table
   is
      function "+" (Source : String) return Document_Type
      is (Alignment_Table_Separator (To_Unbounded_String (Source)));
      --  Converts Source into a Table_Separator command

      function Build_Table return Document_Type;
      --  Creates a compex testing table

      ------------------
      --  Build_Table --
      ------------------

      function Build_Table return Document_Type
      is
         Group_1 : constant Symbol_Type := New_Symbol;
         Group_2 : constant Symbol_Type := New_Symbol;
         Group_3 : constant Symbol_Type := New_Symbol;
         Group_4 : constant Symbol_Type := New_Symbol;

         Table : constant Document_Type :=
           Alignment_Table
             ([[Hard_Line_Without_Break_Parent,
                -- This line break tests that trailing spaces before a table
                -- are removed
                "A ",
                +":",
                Group (Indent (List ([Line, "B "])), Group_1),
                +":=",
                If_Break
                  (Indent (Indent (Group ([Line, "C", ";"]))),
                   Indent (Group ([Line, "C", ";"])),
                   (Group_Id => Group_1))],
               ["AA ",
                +":",
                Group (Indent (List ([Line, "BB "])), Group_2),
                +":=",
                If_Break
                  (Indent (Indent (Group ([Line, "CC", ";"]))),
                   Indent (Group ([Line, "CC", ";"])),
                   (Group_Id => Group_2))],
               ["AAA ",
                +":",
                Group (Indent (List ([Line, "BBBBBBBBB;"])), Group_3)],
                --   BBBBBBBBB tests that it does not affect other separators
               ["AAAA ",
                +":",
                Group (Indent (List ([Line, "BBBB "])), Group_4),
                +":=",
                If_Break
                  (Indent (Indent (Group ([Line, "CCCC", ";"]))),
                   Indent (Group ([Line, "CCCC", ";"])),
                   (Group_Id => Group_4))]]);

         Table_Wrapper : constant Document_Type :=
           Group
             (["record",
               Indent
                 (List
                    ([Hard_Line_Without_Break_Parent,
                      Table])),
               Hard_Line_Without_Break_Parent,
               "end record"]);

      begin
         return Table_Wrapper;
      end Build_Table;

      Alignment_Table_1 : constant Document_Type := Build_Table;
      Alignment_Table_2 : constant Document_Type := Build_Table;
      Alignment_Table_3 : constant Document_Type :=
        Group
          (Alignment_Table
             (Rows =>
                [["with Prettier_Ada.Documents;",
                  List ([List ([+""])]),
                  Group ([Line, "use Prettier_Ada.Documents;"])],
                 ["with Prettier_Ada.Documents.Builders;",
                  List ([+""]),
                  Group ([Line, "use Prettier_Ada.Documents;"])]],
              Must_Break => True));

   begin
      Put_Line ("=== Alignment_Table ===");

      --  Test table alignment without line breaks

      Put_Line ("> Alignment_Table_1 Document JSON:");
      Put_Line (Serialize (Alignment_Table_1));
      Put_Line ("> Alignment_Table_1 Document Formatted:");
      Put_Line (Format (Alignment_Table_1));

      --  Test table alignment with all line breaks

      Put_Line ("> Alignment_Table_2 Document JSON:");
      Put_Line (Serialize (Alignment_Table_2));
      Put_Line ("> Alignment_Table_2 Document Formatted:");
      Put_Line
         (Format
            (Alignment_Table_2,
             (Width       => 1,
              Indentation =>
                (Kind         => Spaces,
                 Width        => 2,
                 Continuation => 2,
                 Offset       => (Spaces => 0, Tabs => 0)),
              End_Of_Line => LF)));

      --  Test that lists are flattened inside table rows

      Put_Line ("> Alignment_Table_3 Document JSON:");
      Put_Line (Serialize (Alignment_Table_3));
      Put_Line ("> Alignment_Table_3 Document Formatted:");
      Put_Line (Format (Alignment_Table_3));
      New_Line;
   end Test_Alignment_Table;

   ------------------------------------
   -- Test_Alignment_Table_Separator --
   ------------------------------------

   procedure Test_Alignment_Table_Separator
   is
      Document : constant Document_Type :=
        List
          (["A",
            " ",
            Alignment_Table_Separator (To_Unbounded_String (":")),
            " ",
            "B",
            " ",
            Alignment_Table_Separator (To_Unbounded_String (":=")),
            " ",
            "C",
            ";"]);

   begin
      Put_Line ("=== Alignment_Table_Separator ===");
      Put_Line ("> Alignment_Table_Separator Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Alignment_Table_Separator Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Alignment_Table_Separator;

   ------------------------
   --  Test_Break_Parent --
   ------------------------

   procedure Test_Break_Parent is
      Document : constant Document_Type :=
        Group (List (["A", Line, "B", Break_Parent]));

   begin
      Put_Line ("=== Break_Parent ===");
      Put_Line ("> Break_Parent Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Break_Parent Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Break_Parent;

   ------------------------------------
   --  Test_Continuation_Line_Indent --
   ------------------------------------

   procedure Test_Continuation_Line_Indent is
      Document : constant Document_Type :=
        Group
          ([Indent
              (List
                 (["begin",
                   Hard_Line,
                   Group
                     (Continuation_Line_Indent
                        (["Foobar", Line, "(Baz);"]))])),
            Hard_Line,
            "end"]);
   begin
      Put_Line ("=== Continuation_Line_Indent ===");
      Put_Line ("> Continuation_Line_Indent Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Continuation_Line_Indent Document Formatted:");
      Put_Line
        (Format
           (Document,
            (Width       => 1,
             Indentation =>
               (Kind         => Spaces,
                Width        => 3,
                Continuation => 2,
                Offset       => (Spaces => 0, Tabs => 0)),
             End_Of_Line => LF)));
      New_Line;
   end Test_Continuation_Line_Indent;

   ------------------
   --  Test_Cursor --
   ------------------

   procedure Test_Cursor is
      Document : constant Document_Type :=
        Group (List (["A", Cursor, "B"]));

   begin
      Put_Line ("=== Cursor ===");
      Put_Line ("> Cursor Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Cursor Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Cursor;

   ----------------
   --  Test_Fill --
   ----------------

   procedure Test_Fill is
      --  Fill always expects a list of documents, even if there's just one
      --  document. Therefore, test that if it's constructed with a document
      --  that it's not a list, it implicitly wraps into one.
      Document_1 : constant Document_Type :=
        Fill (List (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]));
      Document_2 : constant Document_Type :=
        Fill ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA");
      Document_3 : constant Document_Type :=
        Fill
          (List
             (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               Line,
               "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"]));
      Document_4 : constant Document_Type :=
        Fill
          (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
            Line,
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"]);

   begin
      Put_Line ("=== Fill ===");
      Put_Line ("> Fill Document 1 JSON:");
      Put_Line (Serialize (Document_1));
      Put_Line ("> Fill Document 1 Formatted:");
      Put_Line (Format (Document_1));
      Put_Line ("> Fill Document 2 JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> Fill Document 2 Formatted:");
      Put_Line (Format (Document_2));
      Put_Line ("> Fill Document 3 JSON:");
      Put_Line (Serialize (Document_3));
      Put_Line ("> Fill Document 3 Formatted:");
      Put_Line (Format (Document_3));
      Put_Line ("> Fill Document 4 JSON:");
      Put_Line (Serialize (Document_4));
      Put_Line ("> Fill Document 4 Formatted:");
      Put_Line (Format (Document_4));
      New_Line;
   end Test_Fill;

   -----------------
   --  Test_Group --
   -----------------

   procedure Test_Group is
      Document_1 : constant Document_Type :=
        Group
          (List
             (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               Line,
               "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
               Line,
               "C"]));
      Document_2 : constant Document_Type :=
        Group
          (List
             (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               Line,
               "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
               Line,
               "C"]),
           New_Symbol,
           True);
      Document_3 : constant Document_Type :=
        Group
          (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
            Line,
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
            Line,
            "C"]);
      Document_4 : constant Document_Type :=
        Group
          (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
            Line,
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
            Line,
            "C"],
           New_Symbol,
           True);

   begin
      Put_Line ("=== Group ===");
      Put_Line ("> Group Document 1 JSON:");
      Put_Line (Serialize (Document_1));
      Put_Line ("> Group Document 1 Formatted:");
      Put_Line (Format (Document_1));
      Put_Line ("> Group Document 2 JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> Group Document 2 Formatted:");
      Put_Line (Format (Document_2));
      Put_Line ("> Group Document 3 JSON:");
      Put_Line (Serialize (Document_3));
      Put_Line ("> Group Document 3 Formatted:");
      Put_Line (Format (Document_3));
      Put_Line ("> Group Document 4 JSON:");
      Put_Line (Serialize (Document_4));
      Put_Line ("> Group Document 4 Formatted:");
      Put_Line (Format (Document_4));
      New_Line;
   end Test_Group;

   -----------------------------
   --  Test_Conditional_Group --
   -----------------------------

   procedure Test_Conditional_Group is
      Document_1 : constant Document_Type :=
        Conditional_Group
          (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
            "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"]);
      Document_2 : constant Document_Type :=
        Conditional_Group
          ([Text
              (To_Unbounded_String ("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
               & "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
               & "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
               & "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
               & "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"),
            List (["BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB", Line, "CCCCC"])],
            New_Symbol,
            True);
      --  We expect the second alternative to be printed since the first one
      --  does not fit.

   begin
      Put_Line ("=== Conditional_Group ===");
      Put_Line ("> Conditional_Group Document 1 JSON:");
      Put_Line (Serialize (Document_1));
      Put_Line ("> Conditional_Group Document 1 Formatted:");
      Put_Line (Format (Document_1));
      Put_Line ("> Conditional_Group Document 2 JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> Conditional_Group Document 2 Formatted:");
      Put_Line (Format (Document_2));
      New_Line;
   end Test_Conditional_Group;

   ---------------------
   --  Test_Hard_Line --
   ---------------------

   procedure Test_Hard_Line is
      Document : constant Document_Type :=
        Indent (Group (List (["A", Hard_Line, "B", Line, "C"])));

   begin
      Put_Line ("=== Hard_Line ===");
      Put_Line ("> Hard_Line Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Hard_Line Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Hard_Line;

   ------------------------------------------
   --  Test_Hard_Line_Without_Break_Parent --
   ------------------------------------------

   procedure Test_Hard_Line_Without_Break_Parent is
      Document : constant Document_Type :=
        Group (List (["A", Hard_Line_Without_Break_Parent, "B", Line, "C"]));

   begin
      Put_Line ("=== Hard_Line_Without_Break_Parent ===");
      Put_Line ("> Hard_Line_Without_Break_Parent Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Hard_Line_Without_Break_Parent Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Hard_Line_Without_Break_Parent;

   --------------------
   --  Test_If_Break --
   --------------------

   procedure Test_If_Break is
      Document_1 : constant Document_Type :=
        Group
          (List
             (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               Line,
               "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB",
               Line,
               If_Break ("C", "D")]));
      Document_2 : constant Document_Type :=
        Group
          (List
             (["AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
               Line,
               "BBBBBBBBBBBBBBBBBBBB",
               Line,
               If_Break ("C", "D")]));

   begin
      Put_Line ("=== If_Break ===");
      Put_Line ("> If_Break Document 1 JSON:");
      Put_Line (Serialize (Document_1));
      Put_Line ("> If_Break Document 1 Formatted:");
      Put_Line (Format (Document_1));
      Put_Line ("> If_Break Document 2 JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> If_Break Document 2 Formatted:");
      Put_Line (Format (Document_2));
      New_Line;
   end Test_If_Break;

   ---------------------
   --  Test_Indent --
   ---------------------

   procedure Test_Indent is
      Document : constant Document_Type :=
        Indent (List (["A", Line, "B", Line, "C"]));

   begin
      Put_Line ("=== Indent ===");
      Put_Line ("> Indent Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Indent Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Indent;

   ---------------------------
   --  Test_Indent_If_Break --
   ---------------------------

   procedure Test_Indent_If_Break is
      Document : constant Document_Type :=
        Indent_If_Break (Group (List (["A", Hard_Line, "B"])));

   begin
      Put_Line ("=== Indent_If_Break ===");
      Put_Line ("> Indent_If_Break Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Indent_If_Break Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Indent_If_Break;

   ---------------
   -- Test_Join --
   ---------------

   procedure Test_Join is
      Document : constant Document_Type := Join (Hard_Line, ["A", "B"]);

   begin
      Put_Line ("=== Join ===");
      Put_Line ("> Join Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Join Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Join;

   ----------------
   -- Test_Label --
   ----------------

   procedure Test_Label is
      Document : constant Document_Type :=
        Label (To_Unbounded_String ("A"), "B");

   begin
      Put_Line ("=== Label ===");
      Put_Line ("> Label Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Label Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Label;

   ----------------------
   -- Test_Line_Suffix --
   ----------------------

   procedure Test_Line_Suffix is
      Document : constant Document_Type :=
        List (["A", Line_Suffix (" -- comment"), ";", Hard_Line]);

   begin
      Put_Line ("=== Line_Suffix ===");
      Put_Line ("> Line_Suffix Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Line_Suffix Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Line_Suffix;

   -------------------------------
   -- Test_Line_Suffix_Boundary --
   -------------------------------

   procedure Test_Line_Suffix_Boundary is
      Document : constant Document_Type :=
        List
          (["if",
            Line_Suffix (" -- comment"),
            Line_Suffix_Boundary, "then",
            Hard_Line]);

   begin
      Put_Line ("=== Line_Suffix ===");
      Put_Line ("> Line_Suffix Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Line_Suffix Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Line_Suffix_Boundary;

   ---------------
   -- Test_List --
   ---------------

   procedure Test_List is
      Document : constant Document_Type := List (["A", "B"]);

   begin
      Put_Line ("=== List ===");
      Put_Line ("> List Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> List Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_List;

   -----------------------
   -- Test_Literal_Line --
   -----------------------

   procedure Test_Literal_Line is
      Document : constant Document_Type :=
        Indent (Group (List (["A   ", Literal_Line, "B", Line, "C"])));

   begin
      Put_Line ("=== Literal_Line ===");
      Put_Line ("> Literal_Line Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Literal_Line Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Literal_Line;

   --------------------------------------------
   -- Test_Literal_Line_Without_Break_Parent --
   --------------------------------------------

   procedure Test_Literal_Line_Without_Break_Parent is
      Document : constant Document_Type :=
        Indent
          (Group
             (["A   ",
               Literal_Line_Without_Break_Parent,
               "B",
               Line,
               "C"]));

   begin
      Put_Line ("=== Literal_Line_Without_Break_Parent ===");
      Put_Line ("> Literal_Line_Without_Break_Parent Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Literal_Line_Without_Break_Parent Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Literal_Line_Without_Break_Parent;

   ----------------------
   -- Test_No_Document --
   ----------------------

   procedure Test_No_Document is
      Document_1 : Document_Type;
      Document_2 : constant Document_Type :=
        Group ([Document_1, No_Document]);
   begin
      Put_Line ("=== No_Document ===");
      Put_Line ("> No_Document Document JSON:");
      Put_Line (Serialize (Document_2));
      Put_Line ("> No_Document Document Formatted:");
      Put_Line (Format (Document_2));
      New_Line;
   end Test_No_Document;

   -------------------------
   -- Test_String_Literal --
   -------------------------

   procedure Test_String_Literal is
      Document : constant Document_Type := "A";

   begin
      Put_Line ("=== String_Literal ===");
      Put_Line ("> String_Literal Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> String_Literal Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_String_Literal;

   ---------------
   -- Test_Text --
   ---------------

   procedure Test_Text is
      Document : constant Document_Type := Text (To_Unbounded_String ("A"));

   begin
      Put_Line ("=== Text ===");
      Put_Line ("> Text Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Text Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Text;

   ---------------
   -- Test_Trim --
   ---------------

   procedure Test_Trim is
      Document : constant Document_Type :=
        Indent (List (["A", Hard_Line, Trim, "B"]));

   begin
      Put_Line ("=== Trim ===");
      Put_Line ("> Trim Document JSON:");
      Put_Line (Serialize (Document));
      Put_Line ("> Trim Document Formatted:");
      Put_Line (Format (Document));
      New_Line;
   end Test_Trim;

begin
   GNATCOLL.Traces.Parse_Config_File;

   --  Run all the test procedures
   Test_Align;
   Test_Alignment_Table;
   Test_Alignment_Table_Separator;
   Test_Break_Parent;
   Test_Continuation_Line_Indent;
   Test_Cursor;
   Test_Fill;
   Test_Group;
   Test_Conditional_Group;
   Test_Hard_Line;
   Test_Hard_Line_Without_Break_Parent;
   Test_If_Break;
   Test_Indent;
   Test_Indent_If_Break;
   Test_Join;
   Test_Label;
   Test_Line_Suffix;
   Test_Line_Suffix_Boundary;
   Test_List;
   Test_Literal_Line;
   Test_Literal_Line_Without_Break_Parent;
   Test_No_Document;
   Test_String_Literal;
   Test_Text;
   Test_Trim;
end Builders_Tester;
