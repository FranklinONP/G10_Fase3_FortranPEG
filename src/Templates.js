/**
 *
 * @param {{
*  beforeContains: string
*  afterContains: string
*  startingRuleId: string;
*  startingRuleType: string;
*  rules: string[];
*  actions: string[];
* }} data
* @returns {string}
*/
export const main = (data) => `
!auto-generated
module parser
   implicit none
   character(len=:), allocatable, private :: input
   integer, private :: savePoint, lexemeStart, cursor

   interface toStr
       module procedure intToStr
       module procedure strToStr
       module procedure arrayStringToStr
       module procedure arrayIntegerToStr
       module procedure logicalToStr
   end interface
   
   ${data.beforeContains}

   contains
   
   ${data.afterContains}

   function parse(str) result(res)
       character(len=:), allocatable :: str
       ${data.startingRuleType} :: res

       input = str
       cursor = 1

       ${data.startingRuleType.includes('pointer') ? `res => ` : `res = `}${data.startingRuleId}()
   end function parse

   ${data.rules.join('\n')}

   ${data.actions.join('\n')}

   function acceptString(str, peg_isCase) result(accept)
       character(len=*) :: str
       logical :: accept, peg_isCase
       integer :: offset

       offset = len(str) - 1
       if(.not. peg_isCase) then
            if (str /= input(cursor:cursor + offset)) then
                accept = .false.
                return
            end if
        else
            if(tolower(str) /= tolower(input(cursor:cursor + offset))) then
                accept = .false.
                return
            end if
        end if
       cursor = cursor + len(str)
       accept = .true.
   end function acceptString

   function acceptRange(bottom, top, peg_isCase) result(accept)
       character(len=1) :: bottom, top
       logical :: accept, peg_isCase

       if(.not. peg_isCase) then
            if(.not. (input(cursor:cursor) >= bottom .and. input(cursor:cursor) <= top)) then
                accept = .false.
                return
            end if
        else
            if(.not. (tolower(input(cursor:cursor)) >= tolower(bottom) .and. tolower(input(cursor:cursor)) <= tolower(top))) then
                accept = .false.
                return
            end if
        end if
       cursor = cursor + 1
       accept = .true.
   end function acceptRange

   function acceptSet(set, peg_isCase) result(accept)
       character(len=1), dimension(:) :: set
       logical :: accept, peg_isCase

       if(.not. peg_isCase) then
            if(.not. (findloc(set, input(cursor:cursor), 1) > 0)) then
                accept = .false.
                return
            end if
        else
            if(.not. (findloc(set, tolower(input(cursor:cursor)), 1) > 0)) then
                accept = .false.
                return
            end if
        end if
       cursor = cursor + 1
       accept = .true.
   end function acceptSet

   function acceptPeriod() result(accept)
       logical :: accept

       if (cursor > len(input)) then
           accept = .false.
           return
       end if
       cursor = cursor + 1
       accept = .true.
   end function acceptPeriod

   function tolower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        lower_str = str 
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function tolower

   function acceptEOF() result(accept)
       logical :: accept

       if(.not. cursor > len(input)) then
           accept = .false.
           return
       end if
       accept = .true.
   end function acceptEOF

   function consumeInput() result(substr)
       character(len=:), allocatable :: substr

       substr = input(lexemeStart:cursor - 1)
   end function consumeInput

   subroutine pegError()
       print '(A,I1,A)', "Error at ", cursor, ": '"//input(cursor:cursor)//"'"

       call exit(1)
   end subroutine pegError

   function intToStr(int) result(cast)
       integer :: int
       character(len=31) :: tmp
       character(len=:), allocatable :: cast

       write(tmp, '(I0)') int
       cast = trim(adjustl(tmp))
   end function intToStr

   function strToStr(str) result(cast)
       character(len=:), allocatable :: str
       character(len=:), allocatable :: cast

       cast = str
   end function strToStr

   function arrayStringToStr(arr) result(cast)
        character(len=*), intent(in) :: arr(:)
        character(len=:), allocatable :: cast
        integer :: i

        ! Inicializamos la cadena como vacía
        cast = ""

        do i = 1, size(arr)
            if (i == size(arr)) then
                cast = trim(cast) // trim(arr(i))  ! Último elemento sin coma
            else
                cast = trim(cast) // trim(arr(i)) // ", "  ! Elementos separados por coma
            end if
        end do

        ! Agregamos corchetes al resultado
        cast = trim(cast)
    end function arrayStringToStr

    function logicalToStr(log) result(cast)
        logical, intent(in) :: log
        character(len=:), allocatable :: cast
    
        if (log) then
            cast = "true"
        else
            cast = "false"
        end if
    end function logicalToStr

    function arrayIntegerToStr(arr) result(cast)
        integer, intent(in) :: arr(:)
        character(len=:), allocatable :: cast
        character(len=31) :: tmp
        integer :: i

        cast = ""

        do i = 1, size(arr)
            write(tmp, '(I0)') arr(i)
            if (i == size(arr)) then
                cast = trim(cast) // trim(adjustl(tmp))  ! Último elemento sin coma
            else
                cast = trim(cast) // trim(adjustl(tmp)) // ", "  ! Elementos separados por coma
            end if
        end do

        cast = trim(cast)
    end function arrayIntegerToStr
   ! Subroutines for quantifiers 
   ${data.funciones_cuantificadores.join('\n')}
end module parser
`;

/**
*
* @param {{
*  id: string;
*  returnType: string;
*  exprDeclarations: string[];
*  expr: string;
* }} data
* @returns
*/
export const rule = (data) => `
   function peg_${data.id}() result (res)
       ${data.returnType} :: res
       ${data.exprDeclarations.join('\n')}
       integer :: current_size, element_count
       integer :: initial_size = 1, increment_size = 1
       integer :: i
       character(len=:), allocatable :: no_guardado
       integer :: veces,d
        logical :: undefined = .true.


       veces = 0
       d=0

       savePoint = cursor
       ${data.expr}
   end function peg_${data.id}
`;

/**
*
* @param {{
*  exprs: string[]
* }} data
* @returns
*/
export const election = (data) => `
       do i = 0, ${data.exprs.length}
           select case(i)
           ${data.exprs.map(
               (expr, i) => `
           case(${i})
               cursor = savePoint
               ${expr}
               exit
           `
           ).join("")}
           case default
               call pegError()
           end select
       end do
`;

/**
*
* @param {{
*  exprs: string[]
*  startingRule: boolean
*  resultExpr: string
* }} data
* @returns
*/
export const union = (data) => `
               ${data.exprs.join('\n')}
               ${data.startingRule ? 'if (.not. acceptEOF()) cycle' : ''}
               ${data.resultExpr}
`;

/**
*
* @param {{
*  expr: string;
*  destination: string
*  quantifier?: string;
* }} data
* @returns
*/
export const strExpr = (data) => {
    if (!data.quantifier) {
        return `
                lexemeStart = cursor
                if(.not. ${data.expr}) cycle
                ${data.destination} = consumeInput()
        `;
    }
    switch (data.quantifier) {
        case '+':
            return `
                lexemeStart = cursor
                if (.not. ${data.expr}) cycle
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case '*':
            return `
                lexemeStart = cursor
                do while (.not. cursor > len(input))
                    if (.not. ${data.expr}) exit
                end do
                ${data.destination} = consumeInput()
            `;
        case '?':
            return `
                lexemeStart = cursor
                if (${data.expr} .or. .not. (${data.expr})) then
                    continue
                end if
                ${data.destination} = consumeInput()
            `;
        default:
            throw new Error(
                `'${data.quantifier}' quantifier needs implementation`
            );
    }
};



/**
*
* @param {{
    *  expr: string;
    *  destination: string
    *  quantifier?: string;
    * }} data
    * @returns
    */
    export const strExpr2 = (data) => {

        if (!data.qty.tipo) {
            return `
                    lexemeStart = cursor
                    if(.not. ${data.expr}) cycle
                    ${data.destination} = consumeInput()
            `;
        }

        let tipo = data.qty.tipo;
        let min = data.qty.min;
        let max = data.qty.max;

        switch (tipo) {
            case 'unico1':
                return `
                    lexemeStart = cursor
                    veces = 0
                    if (.not. ${data.expr}) then
                        cycle
                    else
                        veces=veces+1
                    end if
                    do d =1, ${min-1}
                        if (.not. (${data.expr})) then
                            exit
                        end if 
                        veces=veces+1
                    end do 
                    if(veces /= ${min}) then
                        veces = 0
                        call pegError()
                    end if 
                    ${data.destination} = consumeInput()
                    `;
            case 'rango1':

                if((min == undefined && max == undefined) || (min == 0 && max == undefined)){
                    return `
                    lexemeStart = cursor
                    do while (.not. cursor > len(input))
                        if (.not. (${data.expr})) then
                            exit
                        end if
                    end do
                    ${data.destination} = consumeInput()
                    `;
                }

                if(min == undefined && max != undefined){
                    return `
                    lexemeStart = cursor
                    veces = 0
                    do d =0, ${max-1}
                        if (.not. (${data.expr})) then
                            exit
                        end if 
                        veces=veces+1
                    end do
                    if(veces > ${max}) then
                        
                        veces = 0
                        call pegError()
                    end if 
                    ${data.destination} = consumeInput()
                    `;
                }

                if(min != undefined && max == undefined){
                    return `
                    lexemeStart = cursor
                    veces = 0
                    if (.not. (${data.expr})) then
                        cycle
                    else
                        veces=veces+1
                    end if
                    do while(.not. cursor > len(input))
                        if (.not. (${data.expr})) then
                            exit
                        end if 
                        veces=veces+1
                    end do
                    if(veces < ${min}) then
                        
                        veces = 0
                        call pegError()
                    end if 
                    ${data.destination} = consumeInput()
                    `;
                }

                if(min == 1 && max == 1){

                    return `
                    lexemeStart = cursor
                    if (.not. (${data.expr})) then
                        cycle
                    end if
                    ${data.destination} = consumeInput()
                    `;

                }else if(max == 1){
                    return `
                    lexemeStart = cursor
                    if (${data.expr} .or. .not. (${data.expr})) then
                        continue
                    end if
                    ${data.destination} = consumeInput()
                    `
                }else{
                    return `
                    lexemeStart = cursor
                    veces = 0
                    if (.not. (${data.expr})) then
                        cycle
                    else
                        veces=veces+1
                    end if
                    do d =0, ${max-2}
                        if (.not. (${data.expr})) then
                            exit
                        end if 
                        veces=veces+1
                    end do 
                    if(veces < ${min} .or. veces > ${max}) then
                        
                        veces = 0
                        call pegError()
                    end if 
                    ${data.destination} = consumeInput()
                    `
                }

            case 'unico2':

                return `
                    lexemeStart = cursor
                    veces = 0
                    if (.not. (${data.expr})) then
                        cycle
                    else
                        veces=veces+1
                    end if 
                    do d =1, ${min-1}
                        if (d < ${min}) then
                            if (.not. acceptString('${data.delimitador}', .false. )) then
                                exit
                            end if
                        end if
                        if (.not. (${data.expr})) then
                            exit
                        end if 
                        veces=veces+1
                    end do
                    if(veces /= ${min}) then
                        
                        veces = 0
                        call pegError()
                    end if 
                    ${data.destination} = consumeInput()`;
            

            case 'rango2':
                
                return ""


            default:
                return `
                    if (.not. (${condition})) then
                        cycle
                    end if
                    `;
        }
    };




/**
*
* @param {{
    *  expr: string;
    *  destination: string
    *  quantifier?: string;
    * }} data
    * @returns
    */
    export const idExpr = (data) => {
        if (!data.quantifier) {
            return `
                    lexemeStart = cursor
                    if(.not. ${data.expr}) cycle
                    ${data.destination} = consumeInput()
            `;
        }
        switch (data.quantifier) {
            case '+':
                let expr = data.expr.split('(')[0]
                return `
                    current_size = initial_size
                    element_count = 0
                    allocate(${data.destination}(current_size)${data.tipo.includes('character') ? `, mold=${data.destination}` : ''})

                    do while ( .not. cursor > len(input))
                        if(element_count == current_size) then
                            call expand_array_${data.id}_${data.destination}(${data.destination}, current_size, increment_size)
                        end if

                        element_count = element_count + 1
                        ${data.destination}(element_count) = ${data.expr}
                    end do
                `;
            case '*':
                return `
                    current_size = initial_size
                    element_count = 0
                    allocate(${data.destination}(current_size)${data.tipo.includes('character') ? `, mold=${data.destination}` : ''})

                    do while ( .not. cursor > len(input))
                        if(element_count == current_size) then
                            call expand_array_${data.id}_${data.destination}(${data.destination}, current_size, increment_size)
                        end if

                        element_count = element_count + 1
                        ${data.destination}(element_count) = ${data.expr}
                    end do
                `;
            default:
                throw new Error(
                    `'${data.quantifier}' quantifier needs implementation`
                );
        }
    };

/**
*
* @param {{
*  exprs: string[];
* }} data
* @returns
*/
export const strResultExpr = (data) => `
               res = ${data.exprs.map((expr) => `toStr(${expr})`).join('//')}
`;

/**
*
* @param {{
*  fnId: string;
*  exprs: string[];
* }} data
* @returns
*/
export const fnResultExpr = (data) => {
    if(data.isPointer) {
        return `res => ${data.fnId}(${data.exprs.join(', ')})`

    }else{ 
        if(data.isLogical){
            return `res = toStr(${data.fnId}(${data.exprs.join(', ')}))`
        }else{
            return `
                res = ${data.fnId}(${data.exprs.join(', ')})`

        }
    }
};

/**
*
* @param {{
*  ruleId: string;
*  choice: number
*  signature: string[];
*  returnType: string;
*  paramDeclarations: string[];
*  code: string;
* }} data
* @returns
*/
export const action = (data) => {
   const signature = data.signature.join(', ');
   return `
   function peg_${data.ruleId}_f${data.choice}(${signature}) result(res)
       ${data.paramDeclarations.join('\n')}
       ${data.returnType} :: res
       ${data.code}
   end function peg_${data.ruleId}_f${data.choice}
   `;
};