(* This file is generated by AutoWp *)

Require Import ZArith Omega Lia.

Open Scope Z_scope.
Lemma div_eucl :
	forall (b a : Z), a >= 0 /\ b > 0 -> a >= 0 /\ a = ((b * 0) + a) /\ (forall (r q : Z), r >= b /\ r >= 0 /\ a = ((b * q) + r) -> (r - b) >= 0 /\ a = ((b * (q + 1)) + (r - b))) /\ (forall (r q : Z), not (r >= b) /\ r >= 0 /\ a = ((b * q) + r) -> r >= 0 /\ r < b /\ a = ((b * q) + r)).
Proof.
	intros. repeat split; intros; lia.
Qed.
