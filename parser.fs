//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a tuple containing 
// 3 values:  
//
//   (result, msg, program)
//
// where result is true or false (legal or not legal), 
// msg is a success or syntax error message, and program
// is a list of instructions if parsing was successful.
//
// <<Raj Patel>>
// U. of Illinois, Chicago
// CS 341, Spring 2019 
// Project #05
//

#light

namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // These are debug routines that output the tokens, or
  // program, respectively.  They are written so you can
  // inject these into a pipeline to output the current 
  // state of the tokens or program.
  //
  let private __outputTokens (tokens, program) =
    printfn "Tokens: %A" tokens
    (tokens, program)

  let private __outputProgram (tokens, program) =
    printfn "Program: %A" program
    (tokens, program)

  
    
    


  //
  // matchToken
  //
  let private matchToken expected_token (tokens, program) =
    let (token, _) = List.head tokens
    //
    // if the token matches the expected token, keep parsing by
    // returning the rest of the tokens.  Otherwise throw an
    // exception because there's a syntax error, effectively 
    // stopping compilation:
    //
    if expected_token = token then    
      (List.tail tokens, program)
    else
      failwith ("expecting " + (string expected_token) + ", but found " + (string token))


  //
  // simpleC
  


  let rec statemnt (tokens,program)=                    //Single Statement
    match tokens with
    | (x,y)::z when  x = lexer.Tokens.Int -> vardec (tokens,["$DECL"]::program)
    | (x,y)::z when  x = lexer.Tokens.Cin -> inputdec (tokens,["$INPUT"]::program)
    | (x,y)::z when  x = lexer.Tokens.Cout -> outputdec (tokens,["$OUTPUT"]::program)
    | (x,y)::z when  x = lexer.Tokens.ID -> assignmentdec (tokens,["$ASSIGN"]::program)
    | (x,y)::z when  x = lexer.Tokens.If -> Ifstats (tokens,["$IF"]::program)
    | (x,y)::z when  x = lexer.Tokens.Semicolon -> EmptyDec (tokens,["$EMPTY"]::program)
    | (x,y)::z when  x = lexer.Tokens.CloseBrace -> nostatement (tokens,program)

  and ExpressionOp (tokens,program)=                //expression operations
      let lst = List.head program  
      match tokens with          
      | (x,y)::z when  x = lexer.Tokens.Plus -> let p = lst@[y] 
                                                matchToken lexer.Tokens.Plus (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Minus -> let p = lst@[y]  
                                                 matchToken lexer.Tokens.Minus (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Times -> let p = lst@[y]  
                                                 matchToken lexer.Tokens.Times (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Divide -> let p = lst@[y] 
                                                  matchToken lexer.Tokens.Divide (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Power -> let p = lst@[y] 
                                                 matchToken lexer.Tokens.Power (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.LT -> let p = lst@[y] 
                                              matchToken lexer.Tokens.LT (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.LTE -> let p = lst@[y] 
                                               matchToken lexer.Tokens.LTE (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.GT -> let p = lst@[y] 
                                              matchToken lexer.Tokens.GT (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.GTE -> let p = lst@[y] 
                                               matchToken lexer.Tokens.GTE (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.EQ -> let p = lst@[y] 
                                              matchToken lexer.Tokens.EQ (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.NE -> let p = lst@[y] 
                                              matchToken lexer.Tokens.NE (tokens,p::(List.tail program))

   and ExpressionVal (tokens,program)=               //expressionValue
      let lst = List.head program
      match tokens with          
      | (x,y)::z when  x = lexer.Tokens.ID -> let p=lst@[(string x);y]
                                              matchToken lexer.Tokens.ID (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Int_Literal ->  let p=lst@[(string x);y]
                                                        matchToken lexer.Tokens.Int_Literal (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Str_Literal ->  let p=lst@[(string x);y]
                                                        matchToken lexer.Tokens.Str_Literal (tokens,p::(List.tail program))
      | (x,y)::z when  x = lexer.Tokens.Bool_Literal -> let p=lst@[(string x);y]
                                                        matchToken lexer.Tokens.Bool_Literal (tokens,p::(List.tail program))


   and outputval (tokens,program)=                   //output value
    let lst = List.head program
    match tokens with          
      |(x,y)::z when  x = lexer.Tokens.Endl ->  let p = lst@[(string x);y]
                                                matchToken lexer.Tokens.Endl (tokens,p::(List.tail program))
      |x::y -> ExpressionVal (tokens,program)
   

   and  vardec (tokens,program)=                     //variable declaration
     let (tokens1,program1)= matchToken lexer.Tokens.Int (tokens,program)
     let (tkn,v) = List.head tokens1
     let lst = List.head program1
     let p= lst@[v]
     (tokens1,p::(List.tail program1))
     |>matchToken lexer.Tokens.ID 
     |>matchToken lexer.Tokens.Semicolon
     
     

   and inputdec (tokens,program)=                       //input dec
     let (T1,P1)=
      (tokens,program)
      |> matchToken lexer.Tokens.Cin
      |> matchToken lexer.Tokens.Input

     let (tkn,v) = List.head T1
     let lst = List.head P1
     let p= lst@[v]

     (T1,p::(List.tail program))
     |> matchToken lexer.Tokens.ID
     |> matchToken lexer.Tokens.Semicolon

   and outputdec (tokens,program)=                   //output dec
     (tokens,program)
     |> matchToken lexer.Tokens.Cout
     |> matchToken lexer.Tokens.Output
     |> outputval
     |>matchToken lexer.Tokens.Semicolon
     
   
   //and let ifstat (tokens,program)=
   //and let conditiondec (tokens,program)=
   //and let thenpart (tokens,program)=
   //and let elsepart (tokens,program)=
   and ExpressionDec (tokens,program)=           // Expression
     let (T1,P1) = ExpressionVal (tokens,program)
     let (t,_) = List.head T1
     if t = lexer.Tokens.Semicolon then
         (T1,P1)

     elif t = lexer.Tokens.CloseParen then
         (T1,P1)
         
     else
         (T1,P1)
         |> ExpressionOp
         |> ExpressionVal

   and assignmentdec (tokens,program)=          //assingnment
     let lst = List.head program
     let (tkn,v)= List.head tokens
     let p = lst@[v]           
     (tokens,p::(List.tail program))
     |> matchToken lexer.Tokens.ID
     |> matchToken lexer.Tokens.Assign
     |> ExpressionDec
     |> matchToken lexer.Tokens.Semicolon

   and EmptyDec (tokens,program)=                //empty func
     let (tkn,_) = List.head tokens
     if tkn = lexer.Tokens.Semicolon then
        let (t1,p1)= matchToken lexer.Tokens.Semicolon (tokens,program)
        (t1,p1)
     else
        (tokens,program)

   and nostatement (tokens,program)=
       let (token, _) = List.head tokens
       (tokens,program)
       |> matchToken lexer.Tokens.CloseBrace

       failwith ("expecting statement, but found " + (string token))
 
  and morestatements (tokens,program)=          //more statements
     let (tkn,_) = List.head tokens
     if tkn = lexer.Tokens.CloseBrace then
        (tokens,program)
     else
        let (t1,p1) = statemnt (tokens,program)
        let (t2,p2) = morestatements (t1,p1)
        (t2,p2)
  and Ifstats (tokens,program)=                     //if statement
        (tokens,program)
        |> matchToken lexer.Tokens.If
        |> matchToken lexer.Tokens.OpenParen
        |> ConditionDec
        |> matchToken lexer.Tokens.CloseParen
        |> ThenPart
        |> ElsePart

  and ConditionDec (tokens,program)=                // Conditional Statement
    (tokens,program)
    |>ExpressionDec

  and ThenPart (tokens,program)=                    // then part
    (tokens,program)
    |>statemnt

  and ElsePart (tokens,program)=                    //Else part
    let (tkn,_)= List.head tokens
    if tkn = lexer.Tokens.Else then                                                             
      let (T1,P1)= matchToken lexer.Tokens.Else (tokens,program)
      statemnt (T1,P1)
    else
    (tokens,["$EMPTY"]::program)

     

  let stments (tokens,program)=                     //multiple statements
    (tokens,program)
    |> statemnt
    |> morestatements


    
    



  //and let morestmts (tokens,program)=
  //and let statements (tokens,program)=
    
  let private simpleC (tokens, program) = 
    (tokens,program)
    |> matchToken lexer.Tokens.Void 
    |> matchToken lexer.Tokens.Main
    |> matchToken lexer.Tokens.OpenParen
    |> matchToken lexer.Tokens.CloseParen 
    |> matchToken lexer.Tokens.OpenBrace 
    |> stments
    |> matchToken lexer.Tokens.CloseBrace 
    |> matchToken lexer.Tokens.EOF 
   
    //matchToken lexer.Tokens.EOF (tokens, program)


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // a tuple containing 3 values:  
  //
  //   (result, msg, program)
  //
  // where result is true or false (legal or not legal), 
  // msg is a success or syntax error message, and program
  // is a list of instructions if parsing was successful.
  //
  let parse tokens = 
    try
      let (_, program) = simpleC (tokens, [])
      (true, "success", List.rev program)
    with 
      | ex -> (false, ex.Message, [])
