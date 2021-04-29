:- module(planogram,
    [ create_product_type/6
]

create_product_type(Name, Gtin, Dimension, Weight, Position) :-
    tell(is_class(C)), tell(has_type(B, C)).


/* 
    {
        "depth" : 85,
        "shelf" : "17",
        "shelflayer" : 5,
        "facing":7,
        "gtin" : "4010355410016",
        "height" : 210,
        "id" : "000000000100002321",
        "length" : 45,
        "name" : "Balea Bodylotion Urea 400ml",
        "weight" : 452
} */