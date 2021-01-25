
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

Lemma conmutatividad_suma (x y : nat) : x + y = y + x.
  Proof.
  induction x.
  simpl. rewrite suma_O. reflexivity.
  simpl. rewrite IHx. rewrite suma_sucesor. reflexivity.
Qed.

(*
Lemma suma_O_inv (x : nat) : suma O x = x.
Proof.
  simpl. reflexivity.
Qed.

Lemma mult_O (x : nat) : mult O x = O.
Proof.
  simpl. reflexivity.
Qed.

Lemma suma_inv (x y : nat): suma S(x) y =*)

Lemma mult_O_suma (x y : nat) : O * (x + y) = O.
Proof.
  induction x.
  rewrite conmutatividad_suma. rewrite suma_O. simpl. reflexivity.
  simpl. reflexivity.
Qed.

Lemma asociatividad_suma (x y z: nat) : (x + y) + z = x + (y + z).
  Proof.
  induction x. 
  simpl. reflexivity.
 simpl. rewrite IHx. reflexivity.
Qed.

(*Lemma auxiliar_suma (x y z : nat) : x + y + z = z + y + x.
Proof.
  induction x.
  simpl. rewrite conmutatividad_suma. rewrite suma_O. reflexivity.
  simpl. rewrite IHx. rewrite <- suma.*)

(*Hypothesis mult_0 : forall (x y : nat), mult 0 (suma x y) = 0.*)

Lemma distributividad (n m r : nat) :
  n * (m + r) = (n * m) + (n * r).
Proof.
  induction n.
  simpl. reflexivity.
  simpl. rewrite IHn. rewrite asociatividad_suma. Check conmutatividad_suma. 
Check conmutatividad_suma (n * m + n * r). Check conmutatividad_suma (n * m + n * r) r. 
rewrite <- conmutatividad_suma.
(*rewrite (conmutatividad_suma r + (n * m + n * r)).
rewrite conmutatividad_suma. 
rewrite asociatividad_suma. *)