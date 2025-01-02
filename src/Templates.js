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
   end interface
   
   ${data.beforeContains}

   contains
   
   ${data.afterContains}

   function parse(str) result(res)
       character(len=:), allocatable :: str
       ${data.startingRuleType} :: res

       input = str
       cursor = 1

       res = ${data.startingRuleId}()
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
       integer :: i

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
            return '! * quantifier not implemented string';
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
                    cursor_actual = cursor
                    lexemeStart = cursor
                    if (.not. ${expr}_logical()) cycle
                    do while (.not. cursor > len(input))
                        if (.not. ${expr}_logical()) exit
                    end do
                    ${data.destination} = consumeInput()
                    cursor = cursor_actual
                `;
            case '*':
                return `
                    len = 0
                    allocate(${data.destination}(0))

                    ${data.destination}_value = ${data.expr}
                    do while ( .not. cursor > len(input))
                        len = size(${data.destination}) + 1
                        if(allocated(${data.destination}_copia)) deallocate(${data.destination}_copia)
                        allocate(${data.destination}_copia(len))
                        ${data.destination}_copia(1:size(${data.destination})) = ${data.destination}
                        ${data.destination}_copia(len) = ${data.destination}_value
                        if(allocated(${data.destination})) deallocate(${data.destination})
                        allocate(${data.destination}(len))
                        ${data.destination}(1:len) = ${data.destination}_copia

                        ${data.destination}_value = ${data.expr}
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
export const fnResultExpr = (data) => `
               res = ${data.fnId}(${data.exprs.join(', ')})
`;

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