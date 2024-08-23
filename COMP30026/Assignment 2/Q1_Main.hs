-- DO NOT MODIFY THIS TYPE
data InferenceRule = ER1 | ER2 | OR1 | NR1 | NR2 | EqL | CaseN
    deriving (Eq, Show)

label1, label2, label3, label4 :: InferenceRule 

label1 = EqL

label2 = ER2

label3 = OR1

label4 = ER1
