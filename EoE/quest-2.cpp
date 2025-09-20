#include <bits/stdc++.h>
using namespace std;
#define ll long long

typedef struct Tree {
  int k;
  char c;
  struct Tree *right, *left;
  Tree(int x, char y) : k{x}, c{y} {}
} Tree;
map<int, vector<Tree *>> tree_id_map;

Tree *add_node(int id, int k, char c, Tree *t) {
  if (t == NULL) {
    Tree *res = new Tree(k, c);
    auto [it, inserted] = tree_id_map.insert({id, vector<Tree *>(1, res)});
    if (!inserted)
      it->second.push_back(res);

    return res;
  }

  if (k == t->k) {
    t->c = c;
  } else if (k > t->k) {
    t->right = add_node(id, k, c, t->right);
  } else {
    t->left = add_node(id, k, c, t->left);
  }
  return t;
}

void swap(int *a, int *b) {
  *a = *a ^ *b;
  *b = *a ^ *b;
  *a = *a ^ *b;
}

void swap(char *a, char *b) {
  *a = *a ^ *b;
  *b = *a ^ *b;
  *a = *a ^ *b;
}

void swap(Tree **a, Tree **b) {
  Tree *t = *a;
  *a = *b;
  *b = t;
}

void tree_swap(int id) {
  Tree *t1 = tree_id_map[id][0];
  Tree *t2 = tree_id_map[id][1];

  swap(&t1->k, &t2->k);
  swap(&t1->c, &t2->c);
}

void tree_swap_branches(int id) {
  Tree *t1 = tree_id_map[id][0];
  Tree *t2 = tree_id_map[id][1];

  swap(&t1->k, &t2->k);
  swap(&t1->c, &t2->c);
  swap(&t1->left, &t2->left);
  swap(&t1->right, &t2->right);
}

void print_tree_(Tree *t) {
  if (t == NULL)
    printf("NULL");
  else {
    printf("[k: %d, c: %c]: (", t->k, t->c);
    print_tree_(t->left);
    printf(") (");
    print_tree_(t->right);
    printf(")");
  }
}

void tree_collect(Tree *t, int rank, vector<tuple<int, int, char>> &v) {
  if (t == NULL)
    return;
  
  tree_collect(t->left, rank+1, v);
  v.push_back({rank, v.size(), t->c});
  tree_collect(t->right, rank+1, v);
}

void tree_collect(Tree *t, vector<tuple<int, int, char>> &v) {
  tree_collect(t, 0, v);
}

void print_tree(Tree *t) {
  print_tree_(t);
  printf("\n");
}

vector<char> get_tree_res(Tree *t) {
  vector<tuple<int, int, char>> v;
  int prev = -1;
  vector<char> res;
  vector<char> wind;

  tree_collect(t, v);
  sort(v.begin(), v.end());

  for (ll i = 0; i < v.size(); i++) {
    if (prev != get<0>(v[i])){
      if (wind.size() > res.size()) {
        res.resize(wind.size());
        copy(wind.begin(), wind.end(), res.begin());
      }
      wind.resize(0);
    }
    prev = get<0>(v[i]);
    wind.push_back(get<2>(v[i]));
  }
  if (wind.size() > res.size()) {
    res.resize(wind.size());
    copy(wind.begin(), wind.end(), res.begin());
  }

  return res;
}

int main() {
  int cmd, id, k1, k2;
  char v1, v2;
  Tree *tl = NULL, *tr = NULL;
  while (cin >> cmd) {
    // ADD
    switch (cmd) {
      case 0:
        cin >> id >> k1 >> v1 >> k2 >> v2;
        tl = add_node(id, k1, v1, tl);
        tr = add_node(id, k2, v2, tr);
        // printf("ADD id: %d, (%d, %c) (%d, %c)\n", id, k1, v1, k2, v2);
        break;
      case 1:
        cin >> id;
        tree_swap_branches(id);
        // printf("SWAP id: %d\n", id);
        break;
      default:
        break;
    }
  }
  // print_tree(tl);
  // print_tree(tr);

  auto resl = get_tree_res(tl);
  auto resr = get_tree_res(tr);
  for (auto i : resl)
    printf("%c", i);
  for (auto i : resr)
    printf("%c", i);
  printf("\n");

  // for (auto [k, v] : tree_id_map) {
  //   printf("%d: ", k);
  //   for (auto i : v)
  //     printf("(%d, %c) ", i->k, i->c);
  //   printf("\n");
  // }
}