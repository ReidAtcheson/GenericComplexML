open Num
module RationalComplex : Gencomplex.Sig  = struct



  let weak_num_of_float f = 
    let is_float_integer fl =
      float_of_int((int_of_float fl)) = fl
    in
    let rec sub_num_of_float fl beg =
      if is_float_integer fl then
        num_of_int (int_of_float fl) // num_of_int beg
      else
        sub_num_of_float (fl*.10.0) (beg*10)
    in
    sub_num_of_float f 1


  let num_of_float fl = 
    let (x,n) = frexp fl in
    let bign  = num_of_int n in
    let two   = num_of_int 2 in
    let exp_two_n = power_num two bign in
    let bigx  = weak_num_of_float x in
    exp_two_n */ bigx


    type ret = num
    type imt = num
    type t   = { re : ret; im : imt;}
    let precision = 1e-15
    let mk re im = {re=re;im=im;}
    let real c = c.re
    let imag c = c.im
    let conj c = {re=c.re; im=(minus_num c.im);}
    let mul c1 c2 = 
      let a=c1.re in
      let b=c1.im in
      let c=c2.re in
      let d=c2.im in    
      {re=a*/c-/b*/d;im=a*/d+/b*/c}
    let add c1 c2 = {re=c1.re +/ c2.re;im=c1.im +/ c2.im}
    let inv c = 
      let a=c.re in
      let b=c.im in
      {re=a//(a*/a+/b*/b);im=minus_num b // (a*/a +/ b*/b)}
    let neg c = 
      let a=c.re in
      let b=c.im in
      {re=minus_num a;im=minus_num b}

		(*WARNING: implemented in floats for now. Must add iterative refinements*)
    (* Use formula: sqrt(a+ib) = sqrt( (|S| + a)/2) + sgn(b)*sqrt( (|S| + b)/2) (b != 0)
     *              sqrt(a)    = sqrt(|S|) i (a<0)
     *              |S| = sqrt(a^2 + b^2) *)
    let sqrt z = 
      let rez = float_of_num z.re in
      let imz = float_of_num z.im in
      let zz  = {Complex.re=rez;im=imz} in
      let sqrtz = Complex.sqrt zz in
      mk (num_of_float sqrtz.Complex.re) (num_of_float sqrtz.Complex.im)
    let abs c = 
          sqrt (mul (conj c) c)


    let almost_equal a b (tol:float) =  (float_of_num (real  (abs  (add (neg b)  a)))) < tol
    let zero   = {re=num_of_int 0;im=num_of_int 0}
    let one    = {re=num_of_int 1;im=num_of_int 0}
    let to_string z = (string_of_num (real z)) ^ " + " ^ (string_of_num (imag z)) ^ "i"

    let re r = mk (num_of_float r) (num_of_int 0)
    let im i = mk (num_of_int 0) (num_of_float i)



end;;
