:- module(planogram,
    [ create_product_type/6
]

:- use_module(library('model/OWL')).

create_product_type(Name, Gtin, Dimension, Weight, Position) :-
    string_concat("Product", "_", Temp),
    string_concat("Temp", Gtin, ProductName),
    tell(is_class(ProductName)), tell(has_type(B, C)).


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