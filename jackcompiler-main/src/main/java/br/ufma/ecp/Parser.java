package br.ufma.ecp;
import br.ufma.ecp.VMWriter.Command;
import br.ufma.ecp.VMWriter.Segment;
import br.ufma.ecp.token.Token;
import br.ufma.ecp.token.TokenType;
import br.ufma.ecp.SymbolTable.Kind;
import br.ufma.ecp.SymbolTable.Symbol;
import static br.ufma.ecp.token.TokenType.*;


public class Parser {
    private static class ParseError extends RuntimeException {}
    private Scanner scan;
    private Token currentToken;
    private Token peekToken;
    private StringBuilder xmlOutput = new StringBuilder();
    private SymbolTable symTable = new SymbolTable();
    private String className;
    private int ifLabelNum; //numero de if
    private int whileLabelNum; //numero de while
    private VMWriter vmWriter = new VMWriter();

    public Parser(byte[] input) {
        scan = new Scanner(input);
        
        nextToken();
        ifLabelNum = 0;
        whileLabelNum = 0;  
    }

    private void nextToken() {
        currentToken = peekToken;
        peekToken = scan.nextToken();
    }

    public void parser() {
        parseClass();
    }

    // 'class' className '{' classVarDec* subroutineDec* '}'
    void parseClass() {
        printNonTerminal("class");
        expectPeek(TokenType.CLASS);
        expectPeek(TokenType.IDENT);
        className = currentToken.lexeme;
        expectPeek(TokenType.LBRACE);

        while (peekTokenIs(TokenType.STATIC) || peekTokenIs(TokenType.FIELD)) {
            parseClassVarDec();
        }

        while (peekTokenIs(TokenType.FUNCTION) || peekTokenIs(TokenType.CONSTRUCTOR) || peekTokenIs(TokenType.METHOD)) {
            parseSubroutineDec();
        }

        expectPeek(TokenType.RBRACE);

        printNonTerminal("/class");
    }

    /**
     * 
     */
    void parseTerm() {
        printNonTerminal("term");
        switch (peekToken.type) {
          case NUMBER:
            expectPeek(TokenType.NUMBER);
            vmWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.lexeme));
                System.out.println(currentToken);
            break;

          case STRING:
            expectPeek(TokenType.STRING);
            var strValue = currentToken.lexeme;
            vmWriter.writePush(Segment.CONST, strValue.length());
            vmWriter.writeCall("String.new", 1);
            for (int i = 0; i < strValue.length(); i++) {
                vmWriter.writePush(Segment.CONST, strValue.charAt(i));
                vmWriter.writeCall("String.appendChar", 2);
                }
            break;

            case IDENT:
                
            expectPeek(TokenType.IDENT);

            Symbol sym = symTable.resolve(currentToken.lexeme);
            
            if (peekTokenIs(TokenType.LPAREN) || peekTokenIs(TokenType.DOT)) {
                parseSubroutineCall();
            } else { 
                if (peekTokenIs(TokenType.LBRACKET)) { // array
                    expectPeek(TokenType.LBRACKET);
                    parseExpression();
                    vmWriter.writePush(kind2Segment(sym.kind()), sym.index());
                    vmWriter.writeArithmetic(Command.ADD);
    

                    expectPeek(TokenType.RBRACKET);
                    vmWriter.writePop(Segment.POINTER, 1); // pop address pointer into pointer 1
                    vmWriter.writePush(Segment.THAT, 0);   // push the value of the address pointer back onto stack
    
                } else {
                    vmWriter.writePush(kind2Segment(sym.kind()), sym.index());
                }
            }
            break;
        case FALSE:
        case NULL:
        case TRUE:
          expectPeek(TokenType.FALSE, TokenType.NULL, TokenType.TRUE);   
          vmWriter.writePush(Segment.CONST, 0);
          if (currentToken.type == TokenType.TRUE)
              vmWriter.writeArithmetic(Command.NOT);  
            break;
          case THIS:
            expectPeek(TokenType.THIS);
            vmWriter.writePush(Segment.POINTER, 0);
            break;
          case LPAREN:
            expectPeek(TokenType.LPAREN);
            parseExpression();
            expectPeek(TokenType.RPAREN);
            break;
          case MINUS:
          case NOT:
            expectPeek(MINUS, NOT);
            var op = currentToken.type;
            parseTerm();
            if (op == MINUS)
                vmWriter.writeArithmetic(Command.NEG);
            else
                vmWriter.writeArithmetic(Command.NOT);
            break;
          default:
            throw error(peekToken, "term expected");
        }

        printNonTerminal("/term");
      }

    
    public boolean isOperator(String oper) {
        return "+-*/<>=~&|".contains(oper);
   }


  
   /** void parseSubroutineCall() {     
        
        if (peekTokenIs(TokenType.LPAREN)) {
            expectPeek(TokenType.LPAREN);
            parseExpressionList();           
            expectPeek(TokenType.RPAREN);            
        } else {            
            expectPeek(TokenType.DOT);
            expectPeek(TokenType.IDENT); 
            
            expectPeek(TokenType.LPAREN);
            parseExpressionList();

            expectPeek(TokenType.RPAREN);
        }       
    }
*/
/**
 * 
 */
void parseSubroutineCall() {     
        

        var nArgs = 0;

        var ident = currentToken.lexeme;
        var symbol = symTable.resolve(ident); // classe ou objeto
        var functionName = ident + ".";

        if (peekTokenIs(TokenType.LPAREN)) { // método da propria classe
        expectPeek(TokenType.LPAREN);
        vmWriter.writePush(Segment.POINTER, 0);
        nArgs = parseExpressionList() + 1;
        TokenType type = TokenType.RPAREN;
        if (peekToken.type == type) {
            nextToken();
            xmlOutput.append(String.format("%s\r\n", currentToken.toString()));
        } else {
            throw error(peekToken, "Expected "+type.name());
        }
        functionName = className + "." + ident;
        } else {
        // pode ser um metodo de um outro objeto ou uma função
            expectPeek(TokenType.DOT);
            expectPeek(TokenType.IDENT); // nome da função

        if (symbol != null) { // é um metodo
            functionName = symbol.type() + "." + currentToken.lexeme;
            vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
            nArgs = 1; // do proprio objeto
        } else {
            functionName += currentToken.lexeme; // é uma função
        }

        expectPeek(TokenType.LPAREN);
        nArgs += parseExpressionList();

        expectPeek(TokenType.RPAREN);
    }

        vmWriter.writeCall(functionName, nArgs);
}


        // 'var' type varName ( ',' varName)* ';'
    void parseVarDec() {

        printNonTerminal("varDec");
        expectPeek(TokenType.VAR);
        
        SymbolTable.Kind kind = Kind.VAR;

        // 'int' | 'char' | 'boolean' | className
        expectPeek(TokenType.INT, TokenType.CHAR, TokenType.BOOLEAN, TokenType.IDENT);
        String type = currentToken.lexeme;

        expectPeek(TokenType.IDENT);
        String name = currentToken.lexeme;
        symTable.define(name, type, kind);

        while (peekTokenIs(TokenType.COMMA)) {
            expectPeek(TokenType.COMMA);
            expectPeek(TokenType.IDENT); 
            
            name = currentToken.lexeme;
            symTable.define(name, type, kind);
        }
        expectPeek(TokenType.SEMICOLON);
        printNonTerminal("/varDec");
    }
    

    // classVarDec → ( 'static' | 'field' ) type varName ( ',' varName)* ';'
    void parseClassVarDec() {
        printNonTerminal("classVarDec");
        expectPeek(TokenType.FIELD, TokenType.STATIC);

        SymbolTable.Kind kind = Kind.STATIC;
        if (currentTokenIs(TokenType.FIELD))
            kind = Kind.FIELD;
        
        // 'int' | 'char' | 'boolean' | className
        expectPeek(TokenType.INT, TokenType.CHAR, TokenType.BOOLEAN, TokenType.IDENT);
        String type = currentToken.lexeme;

        expectPeek(TokenType.IDENT);
        String name = currentToken.lexeme;

        symTable.define(name, type, kind);

        while (peekTokenIs(TokenType.COMMA)) {
            expectPeek(TokenType.COMMA);
            expectPeek(TokenType.IDENT);

            name = currentToken.lexeme;
            symTable.define(name, type, kind);
        }

        expectPeek(TokenType.SEMICOLON);
        printNonTerminal("/classVarDec");
    }

   void parseSubroutineDec() {
        printNonTerminal("subroutineDec");

        ifLabelNum = 0;
        whileLabelNum = 0;

        symTable.startSubroutine();

        expectPeek(TokenType.CONSTRUCTOR, TokenType.FUNCTION, METHOD);
        var subroutineType = currentToken.type;

        if (subroutineType == METHOD) {
            symTable.define("this", className, Kind.ARG);
        }

        // 'int' | 'char' | 'boolean' | className
        expectPeek(TokenType.VOID, TokenType.INT, TokenType.CHAR, TokenType.BOOLEAN, TokenType.IDENT);
        expectPeek(TokenType.IDENT);

        var functionName = className + "." + currentToken.lexeme;

        expectPeek(TokenType.LPAREN);
        parseParameterList();
        expectPeek(TokenType.RPAREN);
        parseSubroutineBody(functionName, subroutineType);

        printNonTerminal("/subroutineDec");
    }
   
    void parseLet() {

        var isArray = false;
        printNonTerminal("letStatement");
        expectPeek(TokenType.LET);
        expectPeek(TokenType.IDENT);
        // **
        var symbol = symTable.resolve(currentToken.lexeme);
        // **
        // array
        if (peekTokenIs(TokenType.LBRACKET)) {
            expectPeek(TokenType.LBRACKET);
            parseExpression();
    
            vmWriter.writePush(kind2Segment(symbol.kind()), symbol.index());
            vmWriter.writeArithmetic(Command.ADD);
            expectPeek(TokenType.RBRACKET);
    
            isArray = true;
        }
    
        expectPeek(TokenType.EQ);
        parseExpression();
    
        if (isArray) {
    
            vmWriter.writePop(Segment.TEMP, 0); 
            vmWriter.writePop(Segment.POINTER, 1);
            vmWriter.writePush(Segment.TEMP, 0); 
            vmWriter.writePop(Segment.THAT, 0); 
    
        } else {
            vmWriter.writePop(kind2Segment(symbol.kind()), symbol.index());
        }
    
        expectPeek(TokenType.SEMICOLON);
        printNonTerminal("/letStatement");
    }


    void parseParameterList() {
        printNonTerminal("parameterList");

        SymbolTable.Kind kind = Kind.ARG;

        if (!peekTokenIs(RPAREN)) // verifica se tem pelo menos uma expressao
        {
            expectPeek(INT, CHAR, BOOLEAN, IDENT);
            String type = currentToken.lexeme;

            expectPeek(IDENT);
            String name = currentToken.lexeme;
            symTable.define(name, type, kind);

            while (peekTokenIs(COMMA)) {
                expectPeek(COMMA);
                expectPeek(INT, CHAR, BOOLEAN, IDENT);
                type = currentToken.lexeme;

                expectPeek(IDENT);
                name = currentToken.lexeme;

                symTable.define(name, type, kind);
            }

        }

        printNonTerminal("/parameterList");
    }

    void parseSubroutineBody(String functionName, TokenType subroutineType) {

        printNonTerminal("subroutineBody");
        expectPeek(TokenType.LBRACE);
        while (peekTokenIs(TokenType.VAR)) {
            parseVarDec();
        }
                var numlocals = symTable.varCount(Kind.VAR);
        vmWriter.writeFunction(functionName, numlocals);
        if (subroutineType == TokenType.CONSTRUCTOR) {
            vmWriter.writePush(Segment.CONST, symTable.varCount(Kind.FIELD));
            vmWriter.writeCall("Memory.alloc", 1);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        if (subroutineType == TokenType.METHOD) {
            vmWriter.writePush(Segment.ARG, 0);
            vmWriter.writePop(Segment.POINTER, 0);
        }

        parseStatements();
        expectPeek(TokenType.RBRACE);
        printNonTerminal("/subroutineBody");
    }
    void parseStatements() {
        printNonTerminal("statements");
        while (peekToken.type == TokenType.WHILE ||
                peekToken.type == TokenType.IF ||
                peekToken.type == TokenType.LET ||
                peekToken.type == TokenType.DO ||
                peekToken.type == TokenType.RETURN) {
            parseStatement();
        }

        printNonTerminal("/statements");
    }

    // letStatement | ifStatement | whileStatement | doStatement | returnStatement
    void parseStatement() {
        switch (peekToken.type) {
            case NUMBER:
                expectPeek(TokenType.NUMBER);
                vmWriter.writePush(Segment.CONST, Integer.parseInt(currentToken.lexeme));
                break;
            case LET:
                parseLet();
                break;
            case WHILE:
                parseWhile();
                break;
            case IF:
                parseIf();
                break;
            case RETURN:
                parseReturn();
                break;
            case DO:
                parseDo();
                break;
            default:
                throw new Error("Expected a statement");
        }
    }

    // ReturnStatement -> 'return' expression? ';'
    void parseReturn() {
        printNonTerminal("returnStatement");
        expectPeek(TokenType.RETURN);
        if (!peekTokenIs(TokenType.SEMICOLON)) {
            parseExpression();
        } else {
            vmWriter.writePush(Segment.CONST, 0);
        }
        expectPeek(TokenType.SEMICOLON);
        vmWriter.writeReturn();

        printNonTerminal("/returnStatement");
    }

    void parseWhile() {
        printNonTerminal("whileStatement");
        // **
        var labelTrue = "WHILE_EXP" + whileLabelNum;
        var labelFalse = "WHILE_END" + whileLabelNum;
        whileLabelNum++;

        vmWriter.writeLabel(labelTrue);
        // **

        expectPeek(TokenType.WHILE);
        expectPeek(TokenType.LPAREN);
        parseExpression();
        // **
        vmWriter.writeArithmetic(Command.NOT);
        vmWriter.writeIf(labelFalse);

        expectPeek(TokenType.RPAREN);
        expectPeek(TokenType.LBRACE);
        parseStatements();
        // **
        vmWriter.writeGoto(labelTrue); // Go back to labelTrue and check condition
        vmWriter.writeLabel(labelFalse); // Breaks out of while loop because ~(condition) is true

        expectPeek(TokenType.RBRACE);
        printNonTerminal("/whileStatement");
    }

    // 'if' '(' expression ')' '{' statements '}' ( 'else' '{' statements '}' )?
    void parseIf() {
        printNonTerminal("ifStatement");

        var labelTrue = "IF_TRUE" + ifLabelNum;
        var labelFalse = "IF_FALSE" + ifLabelNum;
        var labelEnd = "IF_END" + ifLabelNum;

        ifLabelNum++;

        expectPeek(TokenType.IF);
        expectPeek(TokenType.LPAREN);
        parseExpression();
        expectPeek(TokenType.RPAREN);
        // **
        vmWriter.writeIf(labelTrue);
        vmWriter.writeGoto(labelFalse);
        vmWriter.writeLabel(labelTrue);

        // **

        expectPeek(TokenType.LBRACE);
        parseStatements();
        expectPeek(TokenType.RBRACE);

        if (peekTokenIs(TokenType.ELSE)) {
            vmWriter.writeGoto(labelEnd);
        }

        vmWriter.writeLabel(labelFalse);

        if (peekTokenIs(TokenType.ELSE)) {
            expectPeek(TokenType.ELSE);
            expectPeek(TokenType.LBRACE);
            parseStatements();
            expectPeek(TokenType.RBRACE);
            vmWriter.writeLabel(labelEnd);
        }

        printNonTerminal("/ifStatement");
    }

      
       
  
    // 'do' subroutineCall ';'
    void parseDo() {
        printNonTerminal("doStatement");
        expectPeek(TokenType.DO);
        expectPeek(TokenType.IDENT);
        parseSubroutineCall();
        expectPeek(TokenType.SEMICOLON);
        vmWriter.writePop(Segment.TEMP, 0);
        
        printNonTerminal("/doStatement");
    }
    
    // expression -> term (op term)*
    void parseExpression() {
    printNonTerminal("expression");
    parseTerm();
    while (isOperator(peekToken.lexeme) ) {
        var ope = peekToken.type;
        expectPeek(peekToken.type);
        parseTerm();
        compileOperators(ope);
    }
    printNonTerminal("/expression");
}

public void compileOperators(TokenType type) {

    if (type == TokenType.ASTERISK) {
        vmWriter.writeCall("Math.multiply", 2);
    } else if (type == TokenType.SLASH) {
        vmWriter.writeCall("Math.divide", 2);
    } else {
        vmWriter.writeArithmetic(typeOperator(type));
    }
}

private Command typeOperator(TokenType type) {
    if (type == TokenType.PLUS)
        return Command.ADD;
    if (type == TokenType.MINUS)
        return Command.SUB;
    if (type == TokenType.LT)
        return Command.LT;
    if (type == TokenType.GT)
        return Command.GT;
    if (type == TokenType.EQ)
        return Command.EQ;
    if (type == TokenType.AND)
        return Command.AND;
    if (type == TokenType.OR)
        return Command.OR;
    return null;
}

private Segment kind2Segment(Kind kind) {
    if (kind == Kind.STATIC)
        return Segment.STATIC;
    if (kind == Kind.FIELD)
        return Segment.THIS;
    if (kind == Kind.VAR)
        return Segment.LOCAL;
    if (kind == Kind.ARG)
        return Segment.ARG;
    return null;
}

    int parseExpressionList() {
        printNonTerminal("expressionList");

        var nArgs = 0;

        if (!peekTokenIs(TokenType.RPAREN)) // verifica se tem pelo menos uma expressao
    {
        parseExpression();
        nArgs = 1;
    }

     //procurando as demais
    while (peekTokenIs(TokenType.COMMA)) {
        expectPeek(TokenType.COMMA);
        parseExpression();
        nArgs++;
        }

       printNonTerminal("/expressionList");
       return nArgs;
    }

    // funções auxiliares
    public String XMLOutput() {
        return xmlOutput.toString();
    }
    public String VMOutput() {
        return vmWriter.vmOutput();
    }

    private void printNonTerminal(String nterminal) {
        xmlOutput.append(String.format("<%s>\r\n", nterminal));
    }

    boolean peekTokenIs(TokenType type) {
        return peekToken.type == type;
    }

    boolean currentTokenIs(TokenType type) {
        return currentToken.type == type;
    }

    private void expectPeek(TokenType... types) {
        for (TokenType type : types) {
            if (peekToken.type == type) {
                expectPeek(type);
                return;
            }
        }

       throw error(peekToken, "Expected a statement");

    }

    private void expectPeek(TokenType type) {
        if (peekToken.type == type) {
            nextToken();
            xmlOutput.append(String.format("%s\r\n", currentToken.toString()));
        } else {
            throw error(peekToken, "Expected "+type.name());
        }
    }

    private static void report(int line, String where,
        String message) {
            System.err.println(
            "[line " + line + "] Error" + where + ": " + message);
    }

    private ParseError error(Token token, String message) {
        if (token.type == TokenType.EOF) {
            report(token.line, " at end", message);
        } else {
            report(token.line, " at '" + token.lexeme + "'", message);
        }
        return new ParseError();
    }

    

}
