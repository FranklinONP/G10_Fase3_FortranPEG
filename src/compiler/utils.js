import FortranTranslator from './Translator.js';

/** @typedef {import('../visitor/CST.js').Grammar} Grammar*/
/** @typedef {import('../visitor/Visitor.js').default<string>} Visitor*/
/** @typedef {import('../visitor/Visitor.js').ActionTypes} ActionTypes*/

/**
 *
 * @param {Grammar} cst
 */
export function generateParser(cst) {
    /** @type {ActionTypes} */
    const ruleReturnTypes = {};
    for (const rule of cst.rules) {
        rule.expr.exprs.forEach((concat, i) => {
            if (concat.action) {
                const functionId = `peg_${rule.id}_f${i}`;
                ruleReturnTypes[functionId] = concat.action.returnType;
            }
        });
    }

    /** @type {Visitor} */
    const translator = new FortranTranslator(ruleReturnTypes);
    return cst.accept(translator);
}

/**
 *
 * @param {string} ruleId
 * @param {number} choice
 * @returns
 */
export function getActionId(ruleId, choice, func = {}) {
    if(func[ruleId] == undefined) {
        func[ruleId] = `peg_${ruleId}_f${choice}`
        return `peg_${ruleId}_f${choice}`;
    } else {
        return func[ruleId];
    }
}

/**
 *
 * @param {string} functionId
 * @param {ActionTypes} actionReturnTypes
 * @returns
 */
export function getReturnType(functionId, actionReturnTypes, isArray = false, id = undefined, func={}) {

    if(actionReturnTypes[functionId] != undefined && actionReturnTypes[functionId].includes('pointer')) {
        console.log("pinte")
    }

    if (isArray) {

        // if(actionReturnTypes[functionId] != undefined) {
        //     return actionReturnTypes[functionId]+ actionReturnTypes[functionId].includes("allocatable") ? "" : ", allocatable";
        // }

        if(actionReturnTypes[functionId] != undefined) {
            return actionReturnTypes[functionId]+`${(actionReturnTypes[functionId].includes("allocatable") ? "" : ", allocatable")}`;
        }

        return 'character(len=:), allocatable';
    }

    if(func[id] != undefined) {
        for (let key in actionReturnTypes) {
            if (key.includes(id)) {
                return actionReturnTypes[key];
            }
        }
        return actionReturnTypes[func[id]] ?? 'character(len=:), allocatable';
    }

    return actionReturnTypes[functionId] ?? 'character(len=:), allocatable';
}

/**
 *
 * @param {number} choice
 * @param {number} index
 * @returns
 */
export function getExprId(choice, index) {
    return `expr_${choice}_${index}`;
}

/**
 *
 * @param {string} rule
 * @returns
 */
export function getRuleId(rule) {
    return `peg_${rule}`;
}