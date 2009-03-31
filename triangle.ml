(*------------------------------------------------------------------------------

   MiniLight OCaml : minimal global illumination renderer
   Copyright (c) 2006-2008, Harrison Ainsworth / HXA7241.

   http://www.hxa7241.org/

------------------------------------------------------------------------------*)




open Vector3f
let (!!) = Lazy.force



let tolerance_k = 1.0 /. 1024.0 ;;




(**
 * A simple, explicit/non-vertex-shared triangle.
 *
 * Includes geometry and quality.
 *
 * Adapts ray intersection code from:
 * 'Fast, Minimum Storage Ray-Triangle Intersection'
 * Moller, Trumbore;
 * Journal of Graphics Tools, v2 n1 p21, 1997.
 * http://www.acm.org/jgt/papers/MollerTrumbore97/
 *)
type t = {
  vertexs_m : vT array;
  vectors_c : vT array;
  edge0 : vT; edge1 : vT; edge3 : vT;
}

(*
 * @param inBuffer_i (Scanf.Scanning.scanbuf) to read from
 *
 * @invariants
 * - emitivity_m    >= 0
 * - reflectivity_m >= 0 and <= 1
 *)
let make inBuffer_i =
(* construction ------------------------------------------------------------- *)
   (* read vectors in sequence:
      three vertexs, then reflectivity, then emitivity *)
   let vectors_c = let rec readVectors vs i = if i = 0 then vs else
      readVectors ((vRead inBuffer_i) :: vs) (i - 1) in Array.of_list (readVectors [] 5) in
   let vertexs_m = Array.map (Array.get vectors_c) [| 4; 3; 2 |] in
   let edge0 = vertexs_m.(1) -| vertexs_m.(0)
   and edge1 = vertexs_m.(2) -| vertexs_m.(1)
   and  edge3 = vertexs_m.(2) -| vertexs_m.(0) in
     { edge0 = edge0; edge1 = edge1; edge3 = edge3; vertexs_m = vertexs_m;
       vectors_c = vectors_c; }

(* queries ------------------------------------------------------------------ *)
   (**
    * Axis-aligned bounding box of triangle.
    *
    * @return (2 Vector3f array) lower corner and upper corner
    *)
let bound t =
     let expand clamp nudge =
       vZip
         (* include tolerance *)
         (fun a b -> nudge b (((abs_float b) +. a) *. tolerance_k)) vOne
         (* fold to min or max *)
         (vZip clamp t.vertexs_m.(0) (vZip clamp t.vertexs_m.(1) t.vertexs_m.(2)))
     in [| expand min (-.);  expand max (+.) |]

   (**
    * Intersection point of ray with triangle.
    *
    * @param rayOrigin    (Vector3f.vT) ray origin
    * @param rayDirection (Vector3f.vT) ray direction unitized
    * @return float option  Some distance along ray if intersected
    *)
let intersection t rayOrigin rayDirection =

  (* begin calculating determinant -- also used to calculate U parameter *)
  let pvec = vCross rayDirection t.edge3 in
  let det  = vDot t.edge0 pvec in

  (* if determinant is near zero, ray lies in plane of triangle *)
  let epsilon = 0.000001 in
    if (det > -.epsilon) && (det < epsilon) then

      None
    else
      let inv_det = 1.0 /. det in

      (* calculate distance from vertex 0 to ray origin *)
      let tvec = rayOrigin -| t.vertexs_m.(0) in

      (* calculate U parameter and test bounds *)
      let u = (vDot tvec pvec) *. inv_det in
        if (u < 0.0) || (u > 1.0) then

          None
        else
          (* prepare to test V parameter *)
          let qvec = vCross tvec t.edge0 in

          (* calculate V parameter and test bounds *)
          let v = (vDot rayDirection qvec) *. inv_det in
            if (v < 0.0) || (u +. v > 1.0) then

              None
            else
              (* calculate t, ray intersects triangle *)
              let hitDistance = (vDot t.edge3 qvec) *. inv_det in

                (* only allow intersections in the forward ray direction *)
                if hitDistance >= 0. then Some hitDistance else None


(**
* Monte-carlo sample point on triangle.
*
* @param random (FRandom.t) random number generator
* @return (Vector3f.vT) point on the triangle
*)
let samplePoint t random =

  (* make barycentric coords *)
  let barycentrics =
     let sqr1, r2 = (sqrt (FRandom.float random), FRandom.float random) in
     vCreate 1.0 (1.0 -. sqr1) ((1.0 -. r2) *. sqr1)

    (* make position by scaling edges by barycentrics *)
  in vScaleFrame [| t.vertexs_m.(0); t.edge0; t.edge3 |] barycentrics

let normal  t = vUnitize (vCross t.edge0 t.edge1)

let tangent t = vUnitize t.edge0

(* half area of parallelogram *)
let area    t = (0.5 *. vLength (vCross t.edge0 t.edge1))

let reflectivity t = vClamp vZero vOne     t.vectors_c.(1)

let emitivity    t = vClamp vZero vMaximum t.vectors_c.(0)
