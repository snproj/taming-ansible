{-# LANGUAGE LambdaCase #-}
module Semantics.Desugar.DesugarRoles where

import GrammarTypes.AnsibleH


toImport :: String -> Task
toImport s = Atomic {
    atomicAttributeSet = AtomicAttributeSet {},
    modDecl = ImportRole {
        name=SimpleVarString s,
        tasksFrom=SimpleVarString "main",
        handlersFrom=SimpleVarString "main"
    },
    uid = UnsetUID
}

rewriteRuleInlineRoles :: Play -> Play
rewriteRuleInlineRoles p = p {
    roleNames = [],
    tasks = Prelude.map toImport (roleNames p) ++ tasks p
}

rewriteRuleInlineRolesForRD :: RootDir -> RootDir
rewriteRuleInlineRolesForRD rd = rd {
    playbook = map rewriteRuleInlineRoles (playbook rd)
}

