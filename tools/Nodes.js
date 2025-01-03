/** @type {{[node: string]: {[arg: string]: string}}} */
const nodes = {
    Grammar: {
        rules: 'Regla[]',
        globalCode: '?{ before: string; after?: string }',
    },
    Regla: {
        id: 'string',
        expr: 'Opciones',
        alias: '?string',
        start: '?boolean',
    },
    Opciones: { 
        exprs: 'Union[]' 
    },
    Union: { 
        exprs: 'Expresion[]',
        action: '?Predicate'
    },
    String: { 
        val: 'string', 
        isCase: '?boolean' 
    },
    Grupo: { 
        opciones: 'Node' 
    },
    Clase: { 
        chars: '(string|Rango)[]', 
        isCase: '?boolean' 
    },
    Rango: { 
        inicio: 'string', fin: 'string' 
    },
    LiteralRango: { 
        contenido: 'string' 
    },
    Identificador: { 
        id: 'string' 
    },
    Pluck: { 
        labeledExpr: 'Label', 
        pluck: '?boolean' 

    },
    Label: { 
        annotatedExpr: 'Annotated', 
        label: '?string' 
    },
    Annotated: {
        expr: 'Node',
        qty: '?(string|Node)', 
        text: '?boolean' 

    },
    Assertion: { 
        assertion: 'Node' 
    },
    NegAssertion: {
        assertion: 'Node' 
    },
    Predicate: {
        returnType: 'string',
        code: 'string',
        params: '{ [label: string]: string }',
        qty: '?string',
        assert: 'string | undefined',
        tipo: 'string',
    },
    Agrupacion: {
        opciones: 'Node',
    },
    Punto: {},
    Fin: {},
};

export default nodes;
