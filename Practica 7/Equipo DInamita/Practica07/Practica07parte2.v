
From Coq Require Import Classical.
Section LogicaProposicional.

Theorem ModusTollens: forall p q: Prop,  (~p -> ~q) /\ q -> p.
Proof.
intros p q H.
destruct H.
destruct (classic p).
- assumption.
- absurd q.
  --  apply H. assumption.
  -- assumption.
Qed.


Theorem ModusPonens: forall P Q: Prop,  (P -> Q) /\ P -> Q.
Proof.
intros P Q H.
destruct H.
apply H.
apply H0.
Qed.


Theorem ejercicioCuatro:  forall p q r s t u: Prop,  (p -> q /\ r ) -> (r\/~q -> s/\t) -> (t <-> u) -> (p->u).
Proof.
intros p q r s t u .
intros H1.
intro H2.
intro H3.
intro H4.
destruct H1.
apply H4.
destruct H2.
left.
apply H0.
destruct H3.
apply H3.
assumption.
Qed.



Theorem contrapositiva:forall p q:Prop, (~q->~p)->(p->q).
Proof.
intros.
apply NNPP.
intro.
apply H in H1.
contradiction.
Qed.




Theorem ejercicioCinco: forall p q r s t u: Prop,(p->q->r)/\ (p\/s)/\(t->q)/\(~s)->~r->~t.
Proof.
intros.
destruct H.
destruct H1.
destruct H2.
intro.
apply H2 in H4.
apply H in H4.
contradiction.
destruct(classic p).
apply H in H4.
contradiction.
apply H5.
destruct H1.
contradiction.
contradiction.
Qed.


