module Optimization.ValueUsage
( Vars(..)
, storedVars
, getStoredVars
, loadedVars
, getLoadedVars
) where

import qualified Data.Set as Set
import qualified Data.List as List
import Optimization.Data
import Intermediate.Data

type Vars = Set.Set Var

storedVars :: InterInstr -> Vars
storedVars (IGetV m) = Set.singleton (OneVar { vmem = m })
storedVars (IGetAc m ci) = Set.singleton (OneArr { vmem = m, vcIndex = ci })
storedVars (IGetA m i) = Set.singleton (AllArr { vmem = m })
storedVars (IStoreVC mD _) = Set.singleton (OneVar { vmem = mD })
storedVars (IStoreVV mD _) = Set.singleton (OneVar { vmem = mD })
storedVars (IStoreVAc mD _ _) = Set.singleton (OneVar { vmem = mD })
storedVars (IStoreVA mD _ _) = Set.singleton (OneVar { vmem = mD })
storedVars (IStoreVE mD _) = Set.singleton (OneVar { vmem = mD })
storedVars (IStoreAcC mD ciD _) = Set.singleton (OneArr { vmem = mD, vcIndex = ciD })
storedVars (IStoreAcV mD ciD _) = Set.singleton (OneArr { vmem = mD, vcIndex = ciD })
storedVars (IStoreAcAc mD ciD _ _) = Set.singleton (OneArr { vmem = mD, vcIndex = ciD })
storedVars (IStoreAcA mD ciD _ _) = Set.singleton (OneArr { vmem = mD, vcIndex = ciD })
storedVars (IStoreAcE mD ciD _) = Set.singleton (OneArr { vmem = mD, vcIndex = ciD })
storedVars (IStoreAC mD iD _) = Set.singleton (AllArr { vmem = mD })
storedVars (IStoreAV mD iD _) = Set.singleton (AllArr { vmem = mD })
storedVars (IStoreAAc mD iD _ _) = Set.singleton (AllArr { vmem = mD })
storedVars (IStoreAA mD iD _ _) = Set.singleton (AllArr { vmem = mD })
storedVars (IStoreAE mD iD _) = Set.singleton (AllArr { vmem = mD })
storedVars (IBranchLe iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (IBranchEq iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (IBranchLEq iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (ILoopEq iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (ILoopNEq iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (ILoopLe iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars (ILoopLEq iC iT iF) = getStoredVars iC `Set.union` getStoredVars iT `Set.union` getStoredVars iF
storedVars _ = Set.empty

getStoredVars :: [InterInstr] -> Vars
getStoredVars = List.foldl' (flip $ Set.union . storedVars) Set.empty

loadedVars :: InterInstr -> Vars
loadedVars (IGetA _ i) = Set.singleton (OneVar { vmem = i })
loadedVars (IPutV m) = Set.singleton (OneVar { vmem = m })
loadedVars (IPutAc m ci) = Set.singleton (OneArr { vmem = m, vcIndex = ci })
loadedVars (IPutA m i) = Set.insert (OneVar { vmem = i }) $ Set.singleton (AllArr { vmem = m })
loadedVars (IStoreVV _ mS) = Set.singleton (OneVar { vmem = mS })
loadedVars (IStoreVAc _ mS ciS) = Set.singleton (OneArr { vmem = mS, vcIndex = ciS })
loadedVars (IStoreVA _ mS iS) = Set.insert (OneVar { vmem = iS }) $ Set.singleton (AllArr { vmem = mS })
loadedVars (IStoreVE _ eS) = getLoadedVars eS
loadedVars (IStoreAcV _ _ mS) = Set.singleton (OneVar { vmem = mS })
loadedVars (IStoreAcAc _ _ mS ciS) = Set.singleton (OneArr { vmem = mS, vcIndex = ciS })
loadedVars (IStoreAcA _ _ mS iS) = Set.insert (OneVar { vmem = iS }) $ Set.singleton (AllArr { vmem = mS })
loadedVars (IStoreAcE _ _ eS) = getLoadedVars eS
loadedVars (IStoreAC _ iD _) = Set.singleton (OneVar { vmem = iD })
loadedVars (IStoreAV _ iD mS) = Set.insert (OneVar { vmem = iD }) $ Set.singleton (OneVar { vmem = mS })
loadedVars (IStoreAAc _ iD mS ciS) = Set.insert (OneVar { vmem = iD }) $ Set.singleton (OneArr { vmem = mS, vcIndex = ciS })
loadedVars (IStoreAA _ iD mS iS) = Set.insert (OneVar { vmem = iD }) $ Set.insert (OneVar { vmem = iS }) $ Set.singleton (AllArr { vmem = mS })
loadedVars (IStoreAE _ iD eS) = Set.insert (OneVar { vmem = iD }) $ getLoadedVars eS
loadedVars (IAddCV _ mR) = Set.singleton (OneVar { vmem = mR })
loadedVars (IAddCAc _ mR ciR) = Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IAddCA _ mR iR) = Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IAddVV mL mR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IAddVAc mL mR ciR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IAddVA mL mR iR) = Set.insert (OneVar { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IAddAcAc mL ciL mR ciR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IAddAcA mL ciL mR iR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IAddAA mL iL mR iR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (ISubCV _ mR) = Set.singleton (OneVar { vmem = mR })
loadedVars (ISubCAc _ mR ciR) = Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (ISubCA _ mR iR) = Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (ISubVC mL _) = Set.singleton (OneVar { vmem = mL })
loadedVars (ISubVV mL mR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (ISubVAc mL mR ciR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (ISubVA mL mR iR) = Set.insert (OneVar { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (ISubAcC mL ciL _) = Set.singleton (OneArr { vmem = mL, vcIndex = ciL })
loadedVars (ISubAcV mL ciL mR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (ISubAcAc mL ciL mR ciR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (ISubAcA mL ciL mR iR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (ISubAC mL iL _) = Set.insert (OneVar { vmem = iL }) $ Set.singleton (AllArr { vmem = mL })
loadedVars (ISubAV mL iL mR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (ISubAAc mL iL mR ciR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (ISubAA mL iL mR iR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IMulCV _ mR) = Set.singleton (OneVar { vmem = mR })
loadedVars (IMulCAc _ mR ciR) = Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IMulCA _ mR iR) = Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IMulVV mL mR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IMulVAc mL mR ciR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IMulVA mL mR iR) = Set.insert (OneVar { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IMulAcAc mL ciL mR ciR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IMulAcA mL ciL mR iR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IMulAA mL iL mR iR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IDivCV _ mR) = Set.singleton (OneVar { vmem = mR })
loadedVars (IDivCAc _ mR ciR) = Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IDivCA _ mR iR) = Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IDivVC mL _) = Set.singleton (OneVar { vmem = mL })
loadedVars (IDivVV mL mR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IDivVAc mL mR ciR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IDivVA mL mR iR) = Set.insert (OneVar { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IDivAcC mL ciL _) = Set.singleton (OneArr { vmem = mL, vcIndex = ciL })
loadedVars (IDivAcV mL ciL mR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IDivAcAc mL ciL mR ciR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IDivAcA mL ciL mR iR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IDivAC mL iL _) = Set.insert (OneVar { vmem = iL }) $ Set.singleton (AllArr { vmem = mL })
loadedVars (IDivAV mL iL mR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IDivAAc mL iL mR ciR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IDivAA mL iL mR iR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IModCV _ mR) = Set.singleton (OneVar { vmem = mR })
loadedVars (IModCAc _ mR ciR) = Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IModCA _ mR iR) = Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IModVC mL _) = Set.singleton (OneVar { vmem = mL })
loadedVars (IModVV mL mR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IModVAc mL mR ciR) = Set.insert (OneVar { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IModVA mL mR iR) = Set.insert (OneVar { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IModAcC mL ciL _) = Set.singleton (OneArr { vmem = mL, vcIndex = ciL })
loadedVars (IModAcV mL ciL mR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IModAcAc mL ciL mR ciR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IModAcA mL ciL mR iR) = Set.insert (OneArr { vmem = mL, vcIndex = ciL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IModAC mL iL _) = Set.insert (OneVar { vmem = iL }) $ Set.singleton (AllArr { vmem = mL })
loadedVars (IModAV mL iL mR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneVar { vmem = mR })
loadedVars (IModAAc mL iL mR ciR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.singleton (OneArr { vmem = mR, vcIndex = ciR })
loadedVars (IModAA mL iL mR iR) = Set.insert (OneVar { vmem = iL }) $ Set.insert (AllArr { vmem = mL }) $ Set.insert (OneVar { vmem = iR }) $ Set.singleton (AllArr { vmem = mR })
loadedVars (IBranchLe iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (IBranchEq iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (IBranchLEq iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (ILoopEq iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (ILoopNEq iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (ILoopLe iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars (ILoopLEq iC iT iF) = getLoadedVars iC `Set.union` getLoadedVars iT `Set.union` getLoadedVars iF
loadedVars _ = Set.empty

getLoadedVars :: [InterInstr] -> Vars
getLoadedVars = List.foldl' (flip $ Set.union . loadedVars) Set.empty
