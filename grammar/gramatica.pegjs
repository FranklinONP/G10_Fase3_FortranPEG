{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';

    let qty_actual = ""
    let isID = false
}}

gramatica
  = _ code:globalCode? prods:regla+ _ {
    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    prods[0].start = true;
    return new n.Grammar(prods, code);
  }

globalCode
  = "{" before:$(. !"contains")* [ \t\n\r]* "contains" [ \t\n\r]* after:$[^}]* "}" {
    return after ? {before, after} : {before}
  }

regla
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Regla(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    isID = false // REVISAR CUIDADO
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:parsingExpression rest:(_ @parsingExpression !(_ literales? _ "=") )* action:(_ @predicate)? {
    const exprs = [expr, ...rest];
    const labeledExprs = exprs
        .filter((expr) => expr instanceof n.Pluck)
        .filter((expr) => expr.labeledExpr.label);
    if (labeledExprs.length > 0) {
      if(action.tipo == "normal"){
        action.params = labeledExprs.reduce((args, labeled) => {
            const expr = labeled.labeledExpr.annotatedExpr.expr;
            args[labeled.labeledExpr.label] =
                expr instanceof n.Identificador ? expr.id : '';
            return args;
        }, {});
      }
      
        
    }
    return new n.Union(exprs, action);
  }

parsingExpression
  = pluck
  / '!'_ !"{" _ assertion:(match/predicate) _ !"}" {
    return new n.NegAssertion(assertion);
  }
  / '&' _ !"{" assertion:(match/predicate) _ !"}" {
    return new n.Assertion(assertion);
  }
  / "!." {
    return new n.Fin();
  }

pluck
  = pluck:"@"? _ expr:label {
    return new n.Pluck(expr, pluck ? true : false);
  }

label
  = label:(@identificador _ ":")? _ expr:annotated {
    return new n.Label(expr, label);
  }

annotated
  = text:"$"? _ expr:match _ qty:([?+*]/conteo)? {
    qty_actual = ""
    if(isID && qty != undefined) qty_actual = qty
    isID = false
    return new n.Annotated(expr, qty, text ? true : false);
  }

match
  = id:identificador {
    usos.push(id)
    isID = true
    return new n.Identificador(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase ? true : false);
  }
  / "(" _ opciones:opciones _ ")" {return new n.Agrupacion(opciones)}
  / chars:corchetes isCase:"i"? {
    return new n.Clase(chars, isCase ? true : false);
  }
  / "." {
    return new n.Punto();
  }

conteo
  = "|" _ inicio:(numero/identificador) _ "|" 
                        {return {min: inicio, tipo: "unico1"}}
  / "|" _ min:(numero / identificador)? _ ".." _ max:(numero / identificador)? _ "|" 
                        {return {min: min, max: max, tipo: "rango1"}}
  / "|" _ inicio:(numero / identificador)? _ "," _ opciones: $literales _ "|" 
                        {return {min:inicio, opciones: opciones.replace(/['"]/g, ''), tipo: "unico2"}}
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero /identificador)? _ "," _ opciones: opciones _ "|" 
                        { return {min: inicio, max: fin, opciones:opciones, tipo:"rango2"} }  

predicate
  = assert:("!"/"&")? _  "{" [ \t\n\r]* returnType:predicateReturnType code:$[^}]* "}" {
    return new n.Predicate(returnType, code, {}, qty_actual,assert,'normal')
  }
  /assert:("!"/"&")? _ "|" _ "{" [ \t\n\r]* returnType:predicateReturnType code:$[^}]* "}" _ "|" {
    //return new n.Predicate(returnType, code, {}, qty_actual,assert,'unico1')
  }
  /assert:("!"/"&")? _ "|" _ "{" _ [ \t\n\r]* returnType:predicateReturnType code:$[^}]* _ "}" _ ".." _ "{" _ [ \t\n\r]* returnType2:predicateReturnType code2:$[^}]* _ "}" _ "|" {
   // return new n.Predicate(returnType, code, {}, qty_actual,assert,'rango1')
  }
  /assert:("!"/"&")? _ "|" _ "{" _ [ \t\n\r]* returnType:predicateReturnType code:$[^}]* _ "}" _ "," _  opciones:opciones _ "|" {
    //return new n.Predicate(returnType, code, opciones, qty_actual,assert,'unico2')
  }
  /assert:("!"/"&")? _ "|" _ "{" _ [ \t\n\r]* returnType:predicateReturnType code:$[^}]* _ "}" _ ".." _ "{" _ [ \t\n\r]* returnType2:predicateReturnType code2:$[^}]* _ "}" _ ","  _ opciones:opciones _ "|" {
   // return new n.Predicate(returnType, code, opciones, qty_actual,assert,'rango2')
  }

predicateReturnType
  = t:$(. !"::")+ [ \t\n\r]* "::" [ \t\n\r]* "res" {
    return t.trim();
  }


corchetes
    = "[" contenido:(rango / contenido)+ "]" {
        return contenido;
    }

// Regla para validar un rango como [A-Z]
rango
    = inicio:$caracter "-" fin:$caracter {
        return new  n.Rango(inicio, fin);
    }

// Coincide con cualquier contenido que no incluya "]"
contenido
    = contenido: (corchete / @$texto){
        return new n.LiteralRango(contenido);                     //crear nodo literalRango
    }

corchete
    = "[" contenido "]"

texto
    = "\\" escape
    /[^\[\]]


caracter
  = [^\[\]\\]
  / "\\" .

literales
  = '"' @stringDobleComilla* '"'
  / "'" @stringSimpleComilla* "'"

stringDobleComilla = !('"' / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) .
                    / "\\" escape
                    / continuacionLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = [\n\r\u2028\u2029]

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    
numero = [0-9]+

identificador = [_a-z]i[_a-z0-9]i* { return text() }

_ = (Comentarios /[ \t\n\r])*

Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
