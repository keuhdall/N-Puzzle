module Error where
    data Error = NotSolvable | InvalidInput | InvalidSearchType

    instance Show Error where
        show NotSolvable        = "Error : puzzle is not solvable."
        show InvalidInput       = "Error : input is invalid."
        show InvalidSearchType  = "Error : invalid search type."