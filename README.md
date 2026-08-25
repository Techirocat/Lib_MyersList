  
# Lib_MyersList: Functional Random-Access Lists for OCaml

Welcome to **Lib_MyersList**, an OCaml library providing collections of purely functional Random-Access Lists. 

Random-Access Lists combine the best of both worlds: classical linked list operations (`cons`, `head`, `tail`) in constant time, with the ability to efficiently access and update any element in logarithmic time. This library is based on the original work by Eugene W. Myers and expanded with newly proposed variants.

---

## Available Data Structures

This library includes four distinct implementations, allowing you to choose the one that best fits your project's time and memory constraints:

### 1. Basic Myers List
The classic structure introduced by Eugene Myers.
- **Pros:** Maintains `cons`, `head`, and `tail` operations in O(1).
- **Lookup Complexity:** O(log n), specifically `3 log(n) - 5` traversals in the worst case.

### 2. Improved Myers List
An optimized version of the basic structure that changes how sub-regions share cells.
- **Pros:** Much faster lookup without using additional memory. 
- **Lookup Complexity:** Reduced to `2 log(n + 1) - 3` traversals.

### 3. Dense Advanced Myers List
An optimization of the original Advanced Myers List where each logical node is condensed into a single cell containing all its pointers.
- **Pros:** Achieves the **best absolute lookup performance** in terms of traversals. Ideal for systems where pointer traversal is extremely costly (e.g., network accesses).
- **Cons:** It is the structure with the highest memory consumption, and the `cons` operation requires additional traversals.

### 4. IARASS (Improved Applicative Random-Access Stack with Sigma region)
A structure that solves the "double descent" problem of the Improved Myers List by introducing *Skip* cells with an extra pointer (shortcut).
- **Pros:** Maintains O(1) `cons`. Presents an excellent balance: faster lookup than the Improved variant and a lower memory footprint compared to the Advanced Myers List.

---

## Benchmarks

The table below summarizes the performance of each variant for a list of **1,000,000 elements** (tests conducted with Sigma = 2, when applicable):



| Variant | Max Traversals (Worst Case) | Total Execution Time | Memory Usage | O(1) cons? |
|---------|-----------------------------|----------------------|--------------|------------|
| **Basic** | 54 | 57.42 ms | 38.15 MB | ✅ Yes |
| **Improved** | 36 | 77.46 ms | 38.15 MB | ✅ Yes |
| **Advanced** | 36 | 700.22 ms | 53.41 MB | ❌ No |
| **Dense Advanced**| **24** | **60.35 ms** | 75.02 MB | ❌ No |
| **IARASS** | 27 | 65.00 ms | 50.86 MB | ✅ Yes |



## 🚀 Installation

You can install the library locally using the `opam` package manager:

1. Clone this repository:
   ```bash
    git clone https://github.com/Techirocat/Lib_MyersList.git
    cd Lib_MyersList/myers_list
    ```

2. Pin and install the package:
    
    ```bash
    opam pin add myers_list .
    opam install myers_list
    ```
    

## Quick Start

Add `myers_list` to your `dune` file dependencies:

```lisp
(executable
 (name main)
 (libraries myers_list))
```

Basic OCaml example using the **IARASS** structure:

```ocaml
open Myers_list

let () =
  (* Create an empty list with the default Sigma (2) *)
  let my_list = Iarass.empty in
  
  (* Add elements (O(1)) *)
  let l1 = Iarass.cons 10 my_list in
  let l2 = Iarass.cons 20 l1 in
  
  (* Random Access (O(log n)) *)
  match Iarass.get l2 1 with
  | Some v -> Printf.printf "Element at index 1: %d\n" v
  | None -> Printf.printf "Index out of bounds\n"
```

## 📖 Documentation

The full API documentation is available online:

👉 **[(Jump to the current API documentation)](https://techirocat.github.io/Lib_MyersList/myers_list/index.html)**

_(There you will find detailed descriptions of all supported functions such as `map`, `fold`, `filter`, conversions, etc)._

  

## Authors

- Library and Report Authors: Guilherme Guerreiro and Miguel Alvito (Trust Lab, University of Algarve, 2026).