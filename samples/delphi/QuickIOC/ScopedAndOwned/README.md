# Scoped services and owned instances (IOwned&lt;T&gt;)

Two samples of the same model: one with a scope per HTTP request (Horse), one with a scope for the whole
application, which every form uses by default (VCL). There is no database: `TFakeDatabase` keeps in
memory what was committed, and `TFakeUnitOfWork` plays the part of a connection with its own
transaction.

Two services save a customer and an audit entry with the **same** two scoped repositories:

| Service | Its constructor asks for | Result |
|---|---|---|
| `TOwnedAuditService` | `IUnitOfWork`, `ICustomerRepository`, `IOwned<IAuditRepository>` | the audit repository is built in a scope of its own, with another unit of work: the audit entry is saved outside the service's transaction and stays after a rollback |
| `TSameTransactionAuditService` | `IUnitOfWork`, `ICustomerRepository`, `IAuditRepository` | both repositories share the unit of work of the scope: a rollback discards both entries |

Every unit of work logs when it is opened, committed, rolled back and closed, so the log shows which
scope each instance came from and when each scope was freed.

```
ScopedAndOwned
├── Common   the model (ScopedAndOwned.Model) and the registrations (ScopedAndOwned.Registration)
├── Horse    ScopedAndOwnedHorse.dproj: console server, one scope per request
└── VCL      ScopedAndOwnedVcl.dproj: one scope for the application; a screen can have its own
```

Both projects find `Quick.IOC` five folders up (the root of QuickLib) and the shared model in
`..\Common`: nothing has to be added to the IDE's library path. The VCL sample has no other dependency.

## Horse sample

### 1. Tell the project where Horse is

The Horse sample needs [Horse](https://github.com/HashLoad/horse) (tested with 3.3.10; any 3.x with
`Req.Sessions` should do). The project looks for it in the folder given by the **`HORSE_DIR`**
variable, the one that contains Horse's `src` folder. Choose one way:

- **Boss** (the default, nothing to configure): in the `Horse` folder of this sample, run

  ```
  boss install
  ```

  `boss.json` asks for Horse, and Boss downloads it to `Horse\modules\horse`, which is where
  `HORSE_DIR` points by default.

- **A copy of Horse you already have**: define the environment variable `HORSE_DIR` in the IDE, and
  restart the IDE if it was open:
  - Delphi 11 and later: *Tools > Options > IDE > Environment Variables > User System Overrides > New*
  - Delphi 10.x: *Tools > Options > Environment Options > Environment Variables > User System Overrides > New*

  Variable name `HORSE_DIR`, value the Horse folder, for example `C:\Components\horse` (the folder
  that contains `src`, not `src` itself).

- **msbuild**: `msbuild ScopedAndOwnedHorse.dproj /p:HORSE_DIR=C:\Components\horse`

If the compiler stops at `uses Horse` with *File not found: 'Horse.dcu'*, `HORSE_DIR` does not point to
the folder that contains `src`.

### 2. Run it

Open `Horse\ScopedAndOwnedHorse.dproj`, build and run. The `.dpr` calls `GlobalContainer.Build` before
`THorse.Listen`: creating a singleton is not thread-safe, so the singletons are created while the
application is still single-threaded. The server listens on port 9001:

| Route | What it does |
|---|---|
| `POST /owned?name=Ann&rollback=true` | saves with `TOwnedAuditService` |
| `POST /same-transaction?name=Bob&rollback=true` | saves with `TSameTransactionAuditService` |
| `GET /database` | what was committed |

`rollback` is optional: without it, the transaction is committed. With curl:

```
curl -X POST "http://localhost:9001/owned?name=Ann&rollback=true"
curl -X POST "http://localhost:9001/same-transaction?name=Bob&rollback=true"
curl http://localhost:9001/database
```

With PowerShell: `Invoke-RestMethod -Method Post "http://localhost:9001/owned?name=Ann&rollback=true"`.

After the two rollbacks above, `/database` shows no customer and one audit entry, Ann's: her audit entry
was saved by the unit of work of the IOwned's scope, outside the transaction that was rolled back.

The console shows each request:

```
POST /owned name=Ann rollback=true
  UnitOfWork #1 opened (a new connection)        <- the request's scope
  UnitOfWork #2 opened (a new connection)        <- the scope the IOwned opened
  UnitOfWork #1: transaction rolled back
OwnedAuditService: transaction rolled back
  service             -> UnitOfWork #1
  CustomerRepository #1 -> UnitOfWork #1
  AuditRepository #1    -> UnitOfWork #2
  UnitOfWork #2 closed: released by its scope    <- the service is released: its IOwned and its scope go
  UnitOfWork #1 closed: released by its scope    <- the request ends: its scope is freed
```

The comments on the right are not printed; they say where each line comes from.

## VCL sample

Open `VCL\ScopedAndOwnedVcl.dproj`, build and run. Here the first scope, scope A, belongs to the
application: `CreateApplicationScope` creates it in the `.dpr`, before the main form, and it is freed
after every form, when the application ends. Every form uses it by default, so they all share its unit
of work (the application's connection), which the main form opens when it starts.

The main screen opens screens with the four combinations of its two check boxes:

| Own scope | Audit in IOwned | The customer is saved by | The audit entry is saved by |
|---|---|---|---|
| no | no | the application's unit of work | the same one, in the same transaction |
| no | yes | the application's unit of work | the unit of work of the IOwned's scope |
| yes | no | the screen's own unit of work | the same one, in the same transaction |
| yes | yes | the screen's own unit of work | the unit of work of the IOwned's scope |

- `TScreenForm.CreateFor(Application, kind)` uses the application's scope (or the scope given in its last
  parameter); `TScreenForm.CreateWithOwnScope(Application, kind)` creates a scope of its own,
  disconnected from scope A, and frees it when the screen closes.
- Save with and without *Roll back the transaction at the end*, then click *Show the database*.
- Save twice on the same screen: the same units of work answer both times. The IOwned the service
  holds, and its scope, live as long as the screen.
- Close the screens. A screen with a scope of its own closes its unit of work; a screen in scope A only
  releases its service (and the IOwned's scope, if it has one): the application's unit of work stays
  open until the application ends.

## Where to read more

`docs/QuickIOC-Scopes.html` explains how the container implements scopes and owned instances, with
this sample as the running example; open it in a browser.
