import * as CST from '../visitor/CST.js';
import * as Template from '../Templates.js';
import { getActionId, getReturnType, getExprId, getRuleId } from './utils.js';

/**
 * @typedef {import('../visitor/Visitor.js').default<string>} Visitor
 * 
 */
/**
 * @implements {Visitor}
 */
export default class FortranTranslator {

     /** @type {ActionTypes} */
     actionReturnTypes;
     /** @type {string[]} */
     actions;
     /** @type {boolean} */
     translatingStart;
     /** @type {string} */
     currentRule;
     /** @type {number} */
     currentChoice;
     /** @type {number} */
     currentExpr;

    constructor(returnTypes) {
        this.arreglo_grupos = [];
        this.contador_grupos = 0;
        this.arreglo_delimitadores = [];
        this.contador_delimitadores = 0;
        this.actionReturnTypes = returnTypes;
        this.actions = [];
        this.translatingStart = false;
        this.currentRule = '';
        this.currentChoice = 0;
        this.currentExpr = 0;
        // this.funciones = [];
    }
    
    /**
     * @param {CST.Grammar} node
     * @this {Visitor}
     */
    visitGrammar(node) {
        const rules = node.rules.map((rule) => rule.accept(this));

        return Template.main({
            beforeContains: node.globalCode?.before ?? '',
            afterContains: node.globalCode?.after ?? '',
            startingRuleId: getRuleId(node.rules[0].id),
            startingRuleType: getReturnType(
                getActionId(node.rules[0].id, 0),
                this.actionReturnTypes
            ),
            actions: this.actions,
            // funciones_logicas: this.funciones,
            rules,
        });
    }


    /**
     * @param {CST.Regla} node
     * @this {Visitor}
     */
    visitRegla(node) {
        this.currentRule = node.id;
        this.currentChoice = 0;

        if (node.start) this.translatingStart = true;

        const ruleTranslation = Template.rule({
            id: node.id,
            returnType: getReturnType(
                getActionId(node.id, this.currentChoice),
                this.actionReturnTypes
            ),
            exprDeclarations: node.expr.exprs.flatMap((election, i) =>
                election.exprs
                    .filter((expr) => expr instanceof CST.Pluck)
                    .map((label, j) => {
                        const expr = label.labeledExpr.annotatedExpr.expr;

                        if(label.labeledExpr.annotatedExpr.qty != undefined){
                            return `${
                                expr instanceof CST.Identificador
                                    ? getReturnType(
                                        getActionId(expr.id, i),
                                        this.actionReturnTypes,
                                        true
                                    )
                                    : 'character(len=:), allocatable'
                            } :: expr_${i}_${j}(:)
                            ${
                                expr instanceof CST.Identificador
                                    ? getReturnType(
                                        getActionId(expr.id, i),
                                        this.actionReturnTypes,
                                        true
                                    )
                                    : 'character(len=:), allocatable'
                            } :: expr_${i}_${j}_copia(:)
                            integer :: len
                            ${
                                expr instanceof CST.Identificador
                                    ? getReturnType(
                                        getActionId(expr.id, i),
                                        this.actionReturnTypes
                                    )
                                    : 'character(len=:), allocatable'
                            } :: expr_${i}_${j}_value`;
                        }

                        return `${
                            expr instanceof CST.Identificador
                                ? getReturnType(
                                    getActionId(expr.id, i),
                                    this.actionReturnTypes
                                )
                                : 'character(len=:), allocatable'
                        } :: expr_${i}_${j}`;
                    })
            ),
            expr: node.expr.accept(this),
        });

        this.translatingStart = false;

        // this.funciones.push( `
        // function peg_${node.id}_logical() result(accept)
        //     logical :: accept
        //     integer :: i

        //     accept = .false.
        //     ${node.expr.accept(this)}
        //     ${
        //         node.start
        //             ? `
        //             if (.not. acceptEOF()) then
        //                 return
        //             end if
        //             `
        //             : ''
        //     }
        //     accept = .true.
        // end function peg_${node.id}_logical
        // `)

        return ruleTranslation;
    }



    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        return Template.election({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                this.currentChoice++;
                return translation;
            }),
        });
    }



     /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
     visitUnion(node) {
        const matchExprs = node.exprs.filter(
            (expr) => expr instanceof CST.Pluck
        );
        const exprVars = matchExprs.map(
            (_, i) => `expr_${this.currentChoice}_${i}`
        );

        /** @type {string[]} */
        let neededExprs;
        /** @type {string} */
        let resultExpr;
        const currFnId = getActionId(this.currentRule, this.currentChoice);
        if (currFnId in this.actionReturnTypes) {
            neededExprs = exprVars.filter(
                (_, i) => matchExprs[i].labeledExpr.label
            );
            resultExpr = Template.fnResultExpr({
                fnId: getActionId(this.currentRule, this.currentChoice),
                exprs: neededExprs.length > 0 ? neededExprs : [],
            });
        } else {
            neededExprs = exprVars.filter((_, i) => matchExprs[i].pluck);
            resultExpr = Template.strResultExpr({
                exprs: neededExprs.length > 0 ? neededExprs : exprVars,
            });
        }
        this.currentExpr = 0;

        if (node.action) this.actions.push(node.action.accept(this));
        return Template.union({
            exprs: node.exprs.map((expr) => {
                const translation = expr.accept(this);
                if (expr instanceof CST.Pluck) this.currentExpr++;
                return translation;
            }),
            startingRule: this.translatingStart,
            resultExpr,
        });
    }
    
    /**
     * @param {CST.Expresion} node
     * @this {Visitor}
     */
    /*visitExpresion(node) {
        if(node.qty != undefined){
            node.expr.qty = node.qty // inherit quantifier
        }

        const condition = node.expr.accept(this);

        
        if(node.qty!=undefined && node.qty != "?" && node.qty != "*"  && node.qty != "+"){

            let tipo = node.qty.tipo;
            let min = node.qty.min;
            let max = node.qty.max;
            switch (tipo) {
                case 'unico1':
                    return `
                        veces = 0
                        if (.not. ${condition}) then
                            cycle
                        else
                            veces=veces+1
                        end if
                        do d =1, ${min-1}
                            if (.not. (${condition})) then
                                exit
                            end if 
                            veces=veces+1
                        end do 
                        if(veces /= ${min}) then
                            accept = .false.
                            veces = 0
                            return
                        end if `;
                case 'rango1':

                    if((min == undefined && max == undefined) || (min == 0 && max == undefined)){
                        return `
                        do while (.not. cursor > len(input))
                            if (.not. (${condition})) then
                                exit
                            end if
                        end do
                        `;
                    }

                    if(min == undefined && max != undefined){
                        return `
                        veces = 0
                        do d =0, ${max-1}
                            if (.not. (${condition})) then
                                exit
                            end if 
                            veces=veces+1
                        end do
                        if(veces > ${max}) then
                            accept = .false.
                            veces = 0
                            return
                        end if `;
                    }

                    if(min != undefined && max == undefined){
                        return `
                        veces = 0
                        if (.not. (${condition})) then
                            cycle
                        else
                            veces=veces+1
                        end if
                        do while(.not. cursor > len(input))
                            if (.not. (${condition})) then
                                exit
                            end if 
                            veces=veces+1
                        end do
                        if(veces < ${min}) then
                            accept = .false.
                            veces = 0
                            return
                        end if `;
                    }

                    if(min == 1 && max == 1){

                        return `
                        if (.not. (${condition})) then
                            cycle
                        end if
                        `;

                    }else if(max == 1){
                        return `
                        if (${condition} .or. .not. (${condition})) then
                            continue
                        end if`
                    }else{
                        return `
                        veces = 0
                        if (.not. (${condition})) then
                            cycle
                        else
                            veces=veces+1
                        end if
                        do d =0, ${max-2}
                            if (.not. (${condition})) then
                                exit
                            end if 
                            veces=veces+1
                        end do 
                        if(veces < ${min} .or. veces > ${max}) then
                            accept = .false.
                            veces = 0
                            return
                        end if `
                    }

                case 'unico2':
                    let indice = this.contador_delimitadores++;
                let funcion = 
                `function acceptDelimiter_${indice}() result(accept)
                    logical :: accept
                    integer :: i,veces
                    accept = .false.

                    ${node.qty.opciones.accept(this)}

                    accept = .true.
                end function acceptDelimiter_${indice}`

                this.arreglo_delimitadores.push(funcion);

                const delimitador =  `acceptDelimiter_${indice}()`;

                return `
                    veces = 0
                    if (.not. (${condition})) then
                        cycle
                    else
                        veces=veces+1
                    end if 
                    do d =1, ${min-1}
                        if (d < ${min}) then
                            if (.not. ${delimitador}) then
                                exit
                            end if
                        end if
                        if (.not. (${condition})) then
                            exit
                        end if 
                        veces=veces+1
                    end do
                    if(veces /= ${min}) then
                        
                        accept = .false.
                        veces = 0
                        return
                    end if `;
                

                case 'rango2':
                    
                    let indice2 = this.contador_delimitadores++;
                    let funcion2 = 
                    `function acceptDelimiter_${indice2}() result(accept)
                        logical :: accept
                        integer :: i,veces
                        accept = .false.

                        ${node.qty.opciones.accept(this)}

                        accept = .true.
                    end function acceptDelimiter_${indice2}`

                    this.arreglo_delimitadores.push(funcion2);

                    const delimitador2 =  `acceptDelimiter_${indice2}()`;

                    return `
                        veces = 0
                        do d =1, ${max-1}
                            if (.not. (${condition})) then
                                exit
                            end if 
                            if (d < ${min}) then
                                if (.not. ${delimitador2}) then
                                    exit
                                end if
                            end if
                            veces=veces+1
                        end do
                        if(veces < ${min} .or. veces > ${max}) then
                            accept = .false.
                            veces = 0
                            return
                        end if`;


                default:
                    return `
                        if (.not. (${condition})) then
                            cycle
                        end if
                        `;
            }

        }else{
            switch (node.qty) {
                    case '+':
                        return `
                        if (.not. (${condition})) then
                            cycle
                        end if
                        do while (.not. cursor > len(input))
                            if (.not. (${condition})) then
                                exit
                            end if
                        end do
                        `;
                    case '*':
                        return `
                        do while (.not. cursor > len(input))
                            if (.not. (${condition})) then
                                exit
                            end if
                        end do
                        `;
                    case '?':
                        return `
                        if (${condition} .or. .not. (${condition})) then
                            continue
                        end if
                        `;
                    default:
                        return `
                        if (.not. (${condition})) then
                            cycle
                        end if
                        `;
                }
        }

    }*/

    /**
     * @param {CST.Predicate} node
     * @this {Visitor}
     */
    visitPredicate(node) {
        return Template.action({
            ruleId: this.currentRule,
            choice: this.currentChoice,
            signature: Object.keys(node.params),
            returnType: node.returnType,
            paramDeclarations: Object.entries(node.params).map(
                ([label, ruleId]) =>
                    `${getReturnType(
                        getActionId(ruleId, this.currentChoice),
                        this.actionReturnTypes
                    )} :: ${label}`
            ),
            code: node.code,
        });
    }

    /**
     * @param {CST.Pluck} node
     * @this {Visitor}
     */
    visitPluck(node) {
        return node.labeledExpr.accept(this);
    }

    /**
     * @param {CST.Label} node
     * @this {Visitor}
     */
    visitLabel(node) {
        return node.annotatedExpr.accept(this);
    }

    /**
     * @param {CST.Annotated} node
     * @this {Visitor}
     */
    visitAnnotated(node) {
        if (node.qty && typeof node.qty === 'string') {
            if (node.expr instanceof CST.Identificador) {
                // TODO: Implement quantifiers (i.e., ?, *, +)
                return Template.idExpr({
                    quantifier: node.qty,
                    expr: node.expr.accept(this),
                    destination: getExprId(this.currentChoice, this.currentExpr),
                });
            }
            return Template.strExpr({
                quantifier: node.qty,
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        } else if (node.qty) {
            // TODO: Implement repetitions (e.g., |3|, |1..3|, etc...)
            throw new Error('Repetitions not implemented.');
        } else {
            if (node.expr instanceof CST.Identificador) {
                return `${getExprId(
                    this.currentChoice,
                    this.currentExpr
                )} = ${node.expr.accept(this)}`;
            }
            return Template.strExpr({
                expr: node.expr.accept(this),
                destination: getExprId(this.currentChoice, this.currentExpr),
            });
        }
    }

    /**
     * @param {CST.Assertion} node
     * @this {Visitor}
     */
    visitAssertion(node) {
        throw new Error('Method not implemented.');
    }
    
    /**
     * @param {CST.NegAssertion} node
     * @this {Visitor}
     */
    visitNegAssertion(node) {
        throw new Error('Method not implemented.');
    }

    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        const isCase = node.isCase ? '.true.' : '.false.';

        return `acceptString('${node.val}', ${isCase})`;
    }

    /**
     * @param {CST.Grupo} node
     * @this {Visitor}
     */
    visitGrupo(node) {
        node.opciones.qty = node.qty

        const indice = this.contador_grupos++;

        let funcion = 
        `function acceptGroup_${indice}() result(accept)
            logical :: accept
            integer :: i,veces
            accept = .false.

            ${node.opciones.accept(this)}

            accept = .true.
        end function acceptGroup_${indice}`

        this.arreglo_grupos.push(funcion);
        return `acceptGroup_${indice}()`;
    }

    /**
     * @param {CST.Clase} node
     * @this {Visitor}
     */
    visitClase(node) {
        let characterClass = [];

        if (node.isCase) {
            node.chars.forEach((char) => {
                char.isCase = true
            })
        }

        const set = node.chars
            .filter((char) => char instanceof CST.LiteralRango)
            .map((char) => `'${node.isCase ? char.accept(this).toLowerCase() : char.accept(this)}'`);
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}], ${node.isCase ? '.true.' : '.false.'})`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return `(${characterClass.join(' .or. ')})`;
    }



    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        const isCase = node.isCase ? '.true.' : '.false.';
        return `acceptRange('${node.inicio}', '${node.fin}', ${isCase})`;
    }
    /**
     * @param {CST.Identificador} node
     * @this {Visitor}
     */
    visitIdentificador(node) {
        return `peg_${node.id}()`;
    }
    /**
     * @param {CST.Punto} node
     * @this {Visitor}
     */
    visitPunto(node) {
        return 'acceptPeriod()';
    }
    /**
     * @param {CST.Fin} node
     * @this {Visitor}
     */
    visitFin(node) {
        return 'acceptEOF()';
    }

    /**
     * @param {CST.LiteralRango} node
     * @this {Visitor}
     */
    visitLiteralRango(node) {
        return node.contenido;
    }
}
