{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarRoles where

import GrammarTypes.AnsibleGrammarTypes


toImport :: String -> Task
toImport s = Atomic {
    atomicAttributeSet = AtomicAttributeSet {},
    modDecl = ImportRole {
        name=SimpleVarString s,
        tasksFrom=SimpleVarString "main",
        handlersFrom=SimpleVarString "main"
    },
    aUID = UnsetUID
}

rewriteRuleInlineRoles :: Play -> Play
rewriteRuleInlineRoles p = p {
    roleNames = [],
    tasks = Prelude.map toImport (roleNames p) ++ tasks p
}

