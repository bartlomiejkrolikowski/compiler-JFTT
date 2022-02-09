module Intermediate.Data
( InterInstr(..)
) where

-- mem - adres zmiennej (lub komorki o indeksie 0) w tablicy
data InterInstr =
    IGetV { mem::Int } | IGetAc { mem::Int, cIndex::Int } | IGetA { mem::Int, index::Int } |
    IPutC { num::Int } | IPutV { mem::Int } | IPutAc { mem::Int, cIndex::Int } | IPutA { mem::Int, index::Int } |
    IStoreVC { memDst::Int, num::Int } | IStoreVV { memDst::Int, memSrc::Int } |
    IStoreVAc { memDst::Int, memSrc::Int, cIndexSrc::Int } | IStoreVA { memDst::Int, memSrc::Int, indexSrc::Int } | IStoreVE { memDst::Int, exprSrc::[InterInstr] } |
    IStoreAcC { memDst::Int, cIndexDst::Int, num::Int } | IStoreAcV { memDst::Int, cIndexDst::Int, memSrc::Int } | IStoreAcAc { memDst::Int, cIndexDst::Int, memSrc::Int, cIndexSrc::Int } |
    IStoreAcA { memDst::Int, cIndexDst::Int, memSrc::Int, indexSrc::Int } | IStoreAcE { memDst::Int, cIndexDst::Int, exprSrc::[InterInstr] } |
    IStoreAC { memDst::Int, indexDst::Int, num::Int } | IStoreAV { memDst::Int, indexDst::Int, memSrc::Int } | IStoreAAc { memDst::Int, indexDst::Int, memSrc::Int, cIndexSrc::Int } |
    IStoreAA { memDst::Int, indexDst::Int, memSrc::Int, indexSrc::Int } | IStoreAE { memDst::Int, indexDst::Int, exprSrc::[InterInstr] } |
    IConst { num::Int } |
    IAddCV { numL::Int, memR::Int } | IAddCAc { numL::Int, memR::Int, cIndexR::Int } | IAddCA { numL::Int, memR::Int, indexR::Int } |
    IAddVV { memL::Int, memR::Int } | IAddVAc { memL::Int, memR::Int, cIndexR::Int } | IAddVA { memL::Int, memR::Int, indexR::Int } |
    IAddAcAc { memL::Int, cIndexL::Int, memR::Int, cIndexR::Int } | IAddAcA { memL::Int, cIndexL::Int, memR::Int, indexR::Int } |
    IAddAA { memL::Int, indexL::Int, memR::Int, indexR::Int } |
    ISubCV { numL::Int, memR::Int } | ISubCAc { numL::Int, memR::Int, cIndexR::Int } | ISubCA { numL::Int, memR::Int, indexR::Int } |
    ISubVC { memL::Int, numR::Int } | ISubVV { memL::Int, memR::Int } | ISubVAc { memL::Int, memR::Int, cIndexR::Int } | ISubVA { memL::Int, memR::Int, indexR::Int } |
    ISubAcC { memL::Int, cIndexL::Int, numR::Int } | ISubAcV { memL::Int, cIndexL::Int, memR::Int } |
    ISubAcAc { memL::Int, cIndexL::Int, memR::Int, cIndexR::Int } | ISubAcA { memL::Int, cIndexL::Int, memR::Int, indexR::Int } |
    ISubAC { memL::Int, indexL::Int, numR::Int } | ISubAV { memL::Int, indexL::Int, memR::Int } |
    ISubAAc { memL::Int, indexL::Int, memR::Int, cIndexR::Int } | ISubAA { memL::Int, indexL::Int, memR::Int, indexR::Int } |
    IMulCV { numL::Int, memR::Int } | IMulCAc { numL::Int, memR::Int, cIndexR::Int } | IMulCA { numL::Int, memR::Int, indexR::Int } |
    IMulVV { memL::Int, memR::Int } | IMulVAc { memL::Int, memR::Int, cIndexR::Int } | IMulVA { memL::Int, memR::Int, indexR::Int } |
    IMulAcAc { memL::Int, cIndexL::Int, memR::Int, cIndexR::Int } | IMulAcA { memL::Int, cIndexL::Int, memR::Int, indexR::Int } |
    IMulAA { memL::Int, indexL::Int, memR::Int, indexR::Int } |
    IDivCV { numL::Int, memR::Int } | IDivCAc { numL::Int, memR::Int, cIndexR::Int } | IDivCA { numL::Int, memR::Int, indexR::Int } |
    IDivVC { memL::Int, numR::Int } | IDivVV { memL::Int, memR::Int } | IDivVAc { memL::Int, memR::Int, cIndexR::Int } | IDivVA { memL::Int, memR::Int, indexR::Int } |
    IDivAcC { memL::Int, cIndexL::Int, numR::Int } | IDivAcV { memL::Int, cIndexL::Int, memR::Int } |
    IDivAcAc { memL::Int, cIndexL::Int, memR::Int, cIndexR::Int } | IDivAcA { memL::Int, cIndexL::Int, memR::Int, indexR::Int } |
    IDivAC { memL::Int, indexL::Int, numR::Int } | IDivAV { memL::Int, indexL::Int, memR::Int } |
    IDivAAc { memL::Int, indexL::Int, memR::Int, cIndexR::Int } | IDivAA { memL::Int, indexL::Int, memR::Int, indexR::Int } |
    IModCV { numL::Int, memR::Int } | IModCAc { numL::Int, memR::Int, cIndexR::Int } | IModCA { numL::Int, memR::Int, indexR::Int } |
    IModVC { memL::Int, numR::Int } | IModVV { memL::Int, memR::Int } | IModVAc { memL::Int, memR::Int, cIndexR::Int } | IModVA { memL::Int, memR::Int, indexR::Int } |
    IModAcC { memL::Int, cIndexL::Int, numR::Int } | IModAcV { memL::Int, cIndexL::Int, memR::Int } |
    IModAcAc { memL::Int, cIndexL::Int, memR::Int, cIndexR::Int } | IModAcA { memL::Int, cIndexL::Int, memR::Int, indexR::Int } |
    IModAC { memL::Int, indexL::Int, numR::Int } | IModAV { memL::Int, indexL::Int, memR::Int } |
    IModAAc { memL::Int, indexL::Int, memR::Int, cIndexR::Int } | IModAA { memL::Int, indexL::Int, memR::Int, indexR::Int } |
    IBranchLe { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } |  -- true gdy wynik mniejszy od 0
    IBranchEq { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } |  -- true gdy wynik rowny 0
    IBranchLEq { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } | -- true gdy wynik mniejszy lub rowny 0
    ILoopLe { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } |  -- true = ... < 0
    ILoopEq { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } |  -- true = ... == 0
    ILoopNEq { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] } | -- true = ... != 0
    ILoopLEq { instrCond::[InterInstr], instrT::[InterInstr], instrF::[InterInstr] }   -- true = ... <= 0
    deriving (Eq,Show)
