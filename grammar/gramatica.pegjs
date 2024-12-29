{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'

    import * as n from '../visitor/CST.js';
}}

gramatica
  = _ prods:producciones+ _ {
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
    return prods;
  }

producciones
  = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Producciones(id, expr, alias);
  }

opciones
  = expr:union rest:(_ "/" _ @union)* {
    return new n.Opciones([expr, ...rest]);
  }

union
  = expr:expresion rest:(_ @expresion !(_ literales? _ "=") )* {
    return new n.Union([expr, ...rest]);
  }

expresion
  = label:$(etiqueta/varios)? _ expr:expresiones _ qty:($[?+*]/conteo)? {
    return new n.Expresion(expr, label, qty);
  }

etiqueta = ("@")? _ id:identificador _ ":" (varios)?

varios = ("!"/"$"/"@"/"&")

expresiones
  = id:identificador {
    usos.push(id)
    return new n.Identificador(id);
  }
  / val:$literales isCase:"i"? {
    return new n.String(val.replace(/['"]/g, ''), isCase);
  }
  / "(" _ opciones:opciones _ ")" {
    return new n.Grupo(opciones);
  }


  / chars:corchetes isCase:"i"? {
    return new n.Clase(chars, isCase);
  }
  / "." {
    return new n.Punto();
  }
  / "!." {
    return new n.Fin();
  }

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo
  = "|" _ inicio:(numero/identificador) _ "|" 
                        {return {min: inicio, tipo: "unico1"}}
  / "|" _ min:(numero / identificador)? _ ".." _ max:(numero / identificador)? _ "|" 
                        {return {min: min, max: max, tipo: "rango1"}}
  / "|" _ inicio:(numero / identificador)? _ "," _ opciones: opciones _ "|" 
                        {return {min:inicio, opciones: opciones, tipo: "unico2"}}
  / "|" _ inicio:(numero / identificador)? _ ".." _ fin:(numero /identificador)? _ "," _ opciones: opciones _ "|" 
                        { return {min: inicio, max: fin, opciones:opciones, tipo:"rango2"} }  

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido
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
