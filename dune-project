(lang dune 2.8)
(generate_opam_files true)

(name varray)
(source (github art-w/varray))
(license MIT)
(authors "Arthur Wendling")
(maintainers "art.wendling@gmail.com")
(version 0.2)

(package
  (name varray)
  (synopsis "Resizable arrays with fast insertion/deletion")
  (depends
    (ocaml (>= "4.08"))
    (monolith :with-test))
  (description
    "
- O(1) constant time for random access `arr.(i)` and updates `arr.(i) <- v`
- O(1) amortized for `push_front` and `pop_front`, `push_back` and `pop_back` to add or remove an element to the start or the end
- O(sqrt N) for `insert_at arr i x` and `delete_at arr i` to insert or delete an element anywhere else

This datastructure was invented by Goodrich and Kloss and is described in their paper \"Tiered Vectors: Efficient Dynamic Arrays for Rank-Based Sequences\".")
)
