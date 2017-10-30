functor MkSeamFind(structure Seq : SEQUENCE) : SEAMFIND =
struct
  structure Seq = Seq
  open Seq

  (* Remove when you're done! *)
  exception NotYetImplemented

  type gradient = real

  fun findSeam G =
    let fun createRow (G, i) =
          let val EmptyCol = Seq.map (fn x => Seq.empty()) (Seq.nth G 0)
              val EmptyM = Seq.map (fn x => EmptyCol) G
              val EmptyRow = Seq.nth EmptyM 0
              val firstIndexValue = Seq.enum (Seq.nth G 0)
              val firstWithIdxSeq = Seq.map (fn(i, v) => (v, [i])) firstIndexValue (* (int * (int list)) Seq.seq *)

              fun minOfThree ((a,al), (b,bl), (c,cl)) =
                  if ((a<b) andalso (a<c)) then (a, al)
                  else if ((b<a) andalso (b<c)) then (b, bl)
                  else (c, cl)

              fun updateCols (j: int, prevRow, RowG) =
                  let val (leftV, leftList) = if (j=0) then (Real.maxFinite, []) else Seq.nth prevRow (j-1)
                      val (middleV, middleList) = Seq.nth prevRow j
                      val (rightV, rightList) = if (j=(Seq.length RowG)-1) then (Real.maxFinite, []) else Seq.nth prevRow (j+1)
                      val jG = Seq.nth RowG j
                      val (lastMin, lastList) = minOfThree ((leftV, leftList), (middleV, middleList), (rightV, rightList))
                      val updatedList = j::lastList
                  in
                      (jG+lastMin, updatedList)
                  end

              fun updateEachRow (i: int, prevRow: (real * (int list)) Seq.seq) : (real * (int list)) Seq.seq =
                  let val RowG = Seq.nth G i (*int Seq.seq*)
                  in
                    Seq.mapIdx (fn (j, _) => updateCols (j, prevRow, RowG)) EmptyRow
                  end

              fun updateRows (i: int) : (real * (int list)) Seq.seq =
                  case i of
                      0 => firstWithIdxSeq
                    | _ => let val prevRow = updateRows (i-1) (*the i-1th row of M*)
                           in
                            updateEachRow (i, prevRow)
                           end
          in
            updateRows i
          end

        val lastRowi = (Seq.length G) - 1
        val lastRow = createRow (G, lastRowi)
        val (min, result) = Seq.reduce (fn((x, xl), (y, yl)) => if (x>y) then (y, yl) else (x, xl)) (Real.minNormalPos, []) lastRow
    in
      Seq.fromList (List.rev result)
    end

end
