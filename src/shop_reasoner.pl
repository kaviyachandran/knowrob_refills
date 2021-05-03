/**  <module> shop_reasoner

  Copyright (C) 2021 Kaviya Dhanabalachandran
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
      * Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.
      * Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in the
        documentation and/or other materials provided with the distribution.
      * Neither the name of the <organization> nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  @author Kaviya Dhanabalachandran
  @license BSD
*/


:- module(shop_reasoner,
    [
    get_all_shelves(-),
    get_layers_in_shelf(r, -),
    get_all_items_in_facing(r, -),
    get_dimensions(r, -, -, -),
    get_number_of_items_in_facing(r, -),
    get_pose_in_desired_reference_frame(r,r, -,-),
    get_model_path(r, -),
    get_product_location(r, -,-,-,-)
    ]).

%% get_all_shelves(?Shelves) is det.
%
% Gives all the shelves from the scanned store data.
%
% @param Shelves - Shelves from the scanned store
%
get_all_shelves(Shelves) :-
    findall(Shelf,
        (
            has_type(Shelf, dmshop:'DMShelfFrame')
        ), 
    Shelves).

%% get_layers_in_shelf(?Shelf, ?Layers) is det.
%
% Gives all the layers attached to a shelf from the scanned store data.
%
% @param Shelf - Shelf from the scanned store
% @param Layers - Shelf layers of the shelf 
%
get_layers_in_shelf(Shelf, Layers) :-
    findall(Layer,
        (triple(Shelf, soma:hasPhysicalComponent, Layer)),
        Layers).

%% get_all_items_in_facing(?Facing, ?Items) is det.
%
% Gives all the items in the facing from the scanned store data.
%
% @param Facing - Facing instance from the scanned data
% @param Items - Items present in the facing
%
get_all_items_in_facing(Facing, Items) :-
    findall(Item, 
            (
                triple(Facing, rdf:type, shop:'ProductFacingStanding'),
                triple(Facing, shop:productInFacing, Item)
            ),
            Items).

%% get_dimensions(?Object, ?Depth, ?Width, ?Height) is det.
%
% Gives dimensions of the object. The dimensions are in meter
%
% @param Object - Physical object
%
get_dimensions(Object, Depth, Width, Height) :-
    object_dimensions(Object, Depth, Width, Height).

%% get_number_of_items_in_facing(?Facing, ?Quantity) is det.
%
% Gives the number of items in a facing
%
% @param Facing - Facing instance from the scanned data
% @param Quantity - Number of items in the facing
%
get_number_of_items_in_facing(Facing, Quantity) :-
    triple(F, shop:layerOfFacing, Layer),
    aggregate_all(count, triple(F, 'http://knowrob.org/kb/shop.owl#productInFacing',_) , Quantity).

%% get_pose_in_desired_reference_frame(?Object, ?FrameName, ?Translation, ?Rotation)
%
% Gives the object pose in desired reference frame
%
% @param Object - Physical object
% @param FrameName - Desired reference frame
% @param Translation - X, Y, Z position
% @param Rotation - X, Y, Z, W quaternion
%
get_pose_in_desired_reference_frame(Object, FrameName, Translation, Rotation) :-
    is_at(Object, [FrameName, Translation, Rotation]).

get_pose_in_desired_reference_frame(Object, FrameName, Translation, Rotation) :-
    print_message(warning, 'Pass an appropriate reference frame. The error could also be due to the object having no pose asserted').

%% get_model_path(?Object, ?Path)
%
% Gives the model path of the object
%
% @param Object - Physical object
% @param Path - Path of the Cad model in ros package
%
get_model_path(Object, Path) :-
    triple(Object, soma:hasShape, Shape), 
    triple(Shape, dul:hasRegion, ShapeRegion), 
    triple(ShapeRegion, soma:hasFilePath, Path).

%% get_product_pose(?ProductType, ?Item, ?Shelf, ?ShelfLayer, ?Facing)
%
% Gives the location of the item of the given product type. Product type can be Cereal, CareProduct.
% Location of the item is given in terms of shelf, shelf layer and facing
%
get_product_location(ProductType, Item, Shelf, ShelfLayer, Facing) :-
    WorldFrame = 'map',
    subclass_of(Product, ProductType), 
    has_type(Item, Product), 
    triple(Facing ,shop:productInFacing, Item),
    triple(Facing, shop:layerOfFacing, Layer),
    triple(Shelf, soma:hasPhysicalComponent, Layer).