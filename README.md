# Todoapp Using Servant and HTMX

A simple TodoList APP built in Haskell using the Servant framework and HTMX. The app allows creating, listing, and deleting todo items.

---

## Features

- Add new todos
- List all todos
- Get a todo by its ID
- Edit a todo by its ID
- Delete a todo by its ID

---

### Clone and Setup

```bash
git clone https://github.com/truthixify/todoapp-servant.git
cd todoapp-servant
stack build
```

### Run the Server

```bash
stack exec todoapp
```

By default, the application runs on: [http://localhost:8081](http://localhost:8081)

---

## 🗂 Project Structure

```
.
├── app/
│   └── Main.hs             -- Application entry point
├── src/
│   ├── Lib.hs              -- API logic
│   └── Todos/       
│       └── Types.hs        -- Data types
│       └── Api.hs          -- API structure 
│       └── Views.hs        -- HTML view    
├── test/
│   └── Spec.hs             -- Tests
├── todoapp-servant.cabal   -- Cabal configuration
└── ...
└── README.md
```