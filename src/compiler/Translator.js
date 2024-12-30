import * as CST from '../visitor/CST.js';

/**
 * @typedef {import('../visitor/Visitor.js').default<string>} Visitor
 */
/**
 * @implements {Visitor}
 */
export default class FortranTranslator {

    constructor(){
        this.arreglo_grupos = [];
        this.contador_grupos = 0;
        this.arreglo_delimitadores = [];
        this.contador_delimitadores = 0;
    }
    
    /**
     * @param {CST.Producciones} node
     * @this {Visitor}
     */
    visitProducciones(node) {
        return `
        function peg_${node.id}() result(accept)
            logical :: accept
            integer :: i,d,veces

            accept = .false.
            ${node.expr.accept(this)}
            ${
                node.start
                    ? `
                    if (.not. acceptEOF()) then
                        return
                    end if
                    `
                    : ''
            }
            accept = .true.
        end function peg_${node.id}
        `;
    }
    /**
     * @param {CST.Opciones} node
     * @this {Visitor}
     */
    visitOpciones(node) {
        const template = `
        do i = 0, ${node.exprs.length}
            select case(i)
                ${node.exprs
                    .map(
                        (expr, i) => `
                        case(${i})
                            ${expr.accept(this)}
                            exit
                        `
                    )
                    .join('\n')}
            case default
                return
            end select
        end do
        `;
        return template;
    }
    /**
     * @param {CST.Union} node
     * @this {Visitor}
     */
    visitUnion(node) {
        return node.exprs.map((expr) => expr.accept(this)).join('\n');
    }
    /**
     * @param {CST.Expresion} node
     * @this {Visitor}
     */
    visitExpresion(node) {
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
                        do d =1, ${min}
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
                    return `
                        veces = 0
                        do d =0, ${max-1}
                            if (.not. (${condition})) then
                                exit
                            end if 
                            veces=veces+1
                        end do 
                        if(veces < ${min} .or. veces>${max}) then
                            accept = .false.
                            veces = 0
                            return
                        end if `;
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
                    do d =1, ${min}
                        if (.not. (${condition})) then
                            exit
                        end if 
                        if (d < ${min}) then
                            if (.not. ${delimitador}) then
                                exit
                            end if
                        end if
                        veces=veces+1
                    end do
                    if(veces /= ${min}) then
                        
                        accept = .false.
                        veces = 0
                        return
                    end if `;
                

                case 'rango2':
                    return `do while (.true.)
                        if (.not. (${condition})) then
                            exit
                        end if
                    end do`;
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

   
    }
    /**
     * @param {CST.String} node
     * @this {Visitor}
     */
    visitString(node) {
        return `acceptString('${node.val}')`;
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
        const set = node.chars
            .filter((char) => char instanceof CST.LiteralRango)
            .map((char) => `'${char.accept(this)}'`);
        const ranges = node.chars
            .filter((char) => char instanceof CST.Rango)
            .map((range) => range.accept(this));
        if (set.length !== 0) {
            characterClass = [`acceptSet([${set.join(',')}])`];
        }
        if (ranges.length !== 0) {
            characterClass = [...characterClass, ...ranges];
        }
        return characterClass.join(' .or. ');
    }
    /**
     * @param {CST.Rango} node
     * @this {Visitor}
     */
    visitRango(node) {
        return `acceptRange('${node.inicio}', '${node.fin}')`;
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
