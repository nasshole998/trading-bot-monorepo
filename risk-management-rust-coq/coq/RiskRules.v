(* Formalization of a simple Risk Rule Check *)

Require Import ZArith.
Require Import Reals.
Require Import RiskStatePrimitives. (* Import shared primitives *)

(* --- Formalization of a simple Risk Check Logic --- *)

(* Simplified function mirroring the logic of MaxOrderQuantityCheck *)
Definition check_max_order_quantity
    (order_quantity : r_decimal)
    (symbol_max_quantities : list (r_string * r_decimal)) (* Association list for symbol limits *)
    (order_symbol : r_string) :
    r_bool. (* True if check passes, False if it fails *)
  let found_limit := assoc order_symbol symbol_max_quantities in (* Find the limit for the symbol *)
  match found_limit with
  | Some max_qty => (* If a limit exists *)
      order_quantity <= max_qty (* Check if quantity is within limit *)
  | None => (* If no specific limit for the symbol *)
      true (* Pass the check *)
  end.
End.

(* --- Stating Properties (Theorems/Lemmas) --- *)

(* Property Example: If the order quantity exceeds the specified limit, the check fails. *)
Theorem order_quantity_exceeds_limit_fails_check :
  forall (order_quantity max_qty : r_decimal) (symbol : r_string) (symbol_max_quantities : list (r_string * r_decimal)),
    order_quantity > max_qty -> (* If the order quantity is greater than the limit *)
    In (symbol, max_qty) symbol_max_quantities -> (* And this limit is defined for the symbol *)
    check_max_order_quantity order_quantity symbol_max_quantities symbol = false. (* Then the check should return false *)
Proof.
  (* Proof goes here *)
  intros. unfold check_max_order_quantity.
  destruct (assoc symbol symbol_max_quantities).
  (* Case 1: Limit found *)
  simpl. (* Simplify based on 'Some max_qty' *)
  intro H_eq. (* Assume assoc returned Some max_qty = Some limit *)
  subst max_qty. (* Replace max_qty with the limit found by assoc *)
  intuition. (* The hypothesis order_quantity > limit contradicts order_quantity <= limit *)
  (* Case 2: Limit not found *)
  simpl. (* Simplify based on 'None' *)
  intro H_assoc_None. (* Assume assoc returned None *)
  assert (~In (symbol, max_qty) symbol_max_quantities) as H_not_in. { apply not_In_assoc_None; auto. } (* Property relating assoc and In *)
  exfalso. apply H_not_in. assumption. (* Contradiction: we assumed In holds *)
Qed.

(* Note: assoc is from Coq's List library. This theorem relies on properties of assoc and In. *)