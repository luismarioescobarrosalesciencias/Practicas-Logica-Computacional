Require Import Setoid.
Section Naturales.

Inductive nat : Set :=
  | O : nat
  | S : nat -> nat.


Fixpoint suma (a b : nat) : nat :=
  match a with
  | O => b
  | S c => S (suma c b)
  end

where "n + m" := (suma n m).

Fixpoint mult (n m : nat) : nat :=
  match n with
    | O => O
    | S n' => suma m (mult n' m)
  end

where "n * m" := (mult n m).

Lemma suma_O (x : nat) : x + O = x.
Proof.
  induction x.
  simpl. reflexivity.
  simpl. rewrite IHx. reflexivity.
Qed.

Lemma suma_sucesor (x y : nat) : x + (S y) = S (x + y).
  Proof.
  induction x.
  simpl. reflexivity.
  simpl. rewrite IHx. reflexivity.
Qed.

Theorem conmutatividad_suma: forall (x y : nat), x + y = y + x.
  Proof.
  intros x y.
  induction x.
  simpl. rewrite suma_O. reflexivity.
  simpl. rewrite IHx. rewrite suma_sucesor. reflexivity.
Qed.

(*Lemma mult_O_suma (x y : nat) : O * (x + y) = O.
Proof.
  induction x.
  rewrite conmutatividad_suma. rewrite suma_O. simpl. reflexivity.
  simpl. reflexivity.
Qed.*)

Theorem asociatividad_suma: forall (x y z: nat), (x + y) + z = x + (y + z).
  Proof.
  intros x y z.
  induction x. 
  simpl. reflexivity.
  simpl. rewrite IHx. reflexivity.
Qed.

Theorem distributividad_suma: forall (n m r: nat), n * (m + r) = (n * m) + (n * r).
Proof.
  intros n m r.
  induction n.
  simpl. reflexivity.
  simpl. rewrite IHn. rewrite asociatividad_suma. rewrite asociatividad_suma. 
  rewrite conmutatividad_suma with (y := (n * m + n * r)). rewrite asociatividad_suma. 
  rewrite conmutatividad_suma with (y := r). reflexivity.
  Qed.

Section Grupos.

(* Un grupo es un conjunto G junto con una operacion binaria sobre G que combina dos elementos en otro de G. Hay un elemento especial que es llamado el elemento identidad y para cada elemento en G existe uno inverso que bajo la operacion obtiene a la identidad *)

Variable G: Set.

Hypotheses (e:G)  (* Neutro *)
           (g:G -> G -> G) (* Operacion binaria*)
           (h:G -> G). (* Inverso *)

(* Las condiciones anteriores satisfacen las siguientes propiedades: *)
Hypothesis Asoc : forall x y z : G, g x (g y z) = g (g x y) z.

Hypothesis Inv : forall x:G, g (h x) x = e.

Hypothesis Neut : forall x:G, g e x = x.

(* notacion para hacer infija a la operacion binaria *)
Infix "<+>" := g (at level 50, left associativity).


(* Este puede ser ejercicio para lab *)
Theorem NeutIdem: forall x:G, g x x = x -> x = e.
Proof.
intros.
rewrite <- Inv with x.
rewrite <- H at 3.
rewrite Asoc.
rewrite Inv.
rewrite Neut.
trivial.
(*reflexivity.*)
Qed.
Theorem Cancel: forall x y z:G, g x y = g x z -> y = z.
Proof.
intros.
rewrite <- Neut with y.
rewrite <- Inv with x.
rewrite <- Asoc.
rewrite H.
rewrite Asoc.
rewrite Inv.
rewrite Neut.
reflexivity.
Qed. (**)

(*Lemma Neut_aux : forall x : G, g x e = x.
Proof.
  intros.
  rewrite <- Inv with x.
  rewrite Asoc.
  *)

(*
Theorem unicidad_invers: forall (x y : G), g x y = e -> y = h x.
Proof.
  intros.
  rewrite <- Neut with y.
  rewrite <- Inv with x.
  rewrite <- Asoc.
  rewrite H.
  reflexivity.
Qed.*)


(* Esto se tiene que probar con la unicidad*)
Theorem InvDer: forall x: G, g x (h x) = e.
Proof.
  intros.
  apply NeutIdem.
rewrite <- Asoc.
rewrite Asoc with (h x) (x) (h x).
rewrite Inv.
rewrite Neut.
reflexivity.
Qed.

(*Hypothesis Inv1 : forall x:G, g x (h x) = e.*)

Theorem NeutDer : forall x:G, g x e = x.
Proof.
  intros.
  cut (e = g (h x) x).
  intro.
  rewrite H.
  rewrite Asoc.
  rewrite InvDer.
  rewrite Neut.
  trivial.
  rewrite Inv.
  trivial.
Qed.

Theorem unicinv: forall z x : G, g z x = e -> x = h z.
Proof.
  intros.
  rewrite <- Neut with x.
  rewrite <- Inv with z.
  rewrite <- Asoc.
  rewrite H.
  rewrite NeutDer.
  reflexivity.
Qed.

(*modus ponens*)

Theorem doble_inverso: forall x : G, h (h x) = x.
  Proof.
  intro x.
  rewrite <- Neut with (h (h x)).
  rewrite <- InvDer with x.
  rewrite <- Asoc.
  rewrite InvDer.
  rewrite NeutDer.
  trivial.
Qed.
