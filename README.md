Running the Backend
-------------------

To install:
1. clone the repo wherever you want,
2. run `make pulldeps`, to download dependencies from GitHub/GitLab,
   (can take about 30 seconds) and then if all the dependencies clone
   correctly,
3. run `make` to compile all of the erlang code. (can take 2-3 minutes from
   scratch)

After that you should be able to run `make run`, and view a placeholder poll
list at `http://localhost:8080/`.

If you want to connect to the chain, either host a testnet node at localhost,
or configure the file called `peerlist` to point to any other testnet node/s.
You can also set the network in that file, but the backend has not been tested
on mainnet at all yet.

API
---

The backend includes an HTTP/JSON API, which is exposed at `http://localhost:8080/api`.

There are three kinds of endpoint:
- public endpoints that you can freely `GET`,
- transaction endpoints that come in pairs, one to form transactions, and
  another to post transactions,
- admin endpoints that come in pairs, one to generate a message to sign,
  corresponding to a given admin action, and another to post the signature.

### Public endpoints:

```
GET /api/getPolls[?category={hidden|all|approved|official}]

response: {
    polls: Array<{
        id: number,
        category: "hidden" | "all" | "approved" | "official",
        title: string,
        close_height: "never_closes" | number,
        closed: boolean,
        options: Array<{
            id: number,
            name: string,
            score: number
        }>
    }>
}
```

This should be roughly what is needed to display a list of all in a given
category. In the example frontend the page that shows the polls uses the same
query string as this endpoint, so that it can be glued directly onto the
request. If no category is specified, it defaults to all, i.e. all but the
hidden polls. Categories form a hierarchy, all official polls show up in
requests for approved polls, all approved polls show up in requests for "all"
polls, and all polls hidden or not show up in requests for "hidden" polls.

```
GET /api/poll/:index

response: {
    id: number,
    title: string,
    description: string,
    url: string,
    close_height: "never_closes" | number,
    closed: boolean,
    options: Array<{
        id: number,
        name: string,
        score: number
    }>
}
```

Similar to the information in the poll list, but with additional text fields
for building a page for one individual poll. These text fields could be moved
into the `/api/getPolls` lists if necessary.

```
GET /api/poll/:index/user/:user

response: {
    pending_vote: "none" | "revoke" | number,
    current_vote: "none" | number
}
```

Get the specific status of a user's vote on a specific poll. `pending_vote`
indicates that a transaction is being mined, whereas `current_vote` indicates
that they have successfully voted for something in the past. Both may be set
independently, if they are voting for the second or third time, etc.

```
GET /api/poll/:index/option/:option

response: {
    name: string,
    votes: Array<{
        weight: number,
        voter_address: string
    }>,
    score: number
}
```

Get detailed information about an option in a poll, namely the weight and
address of all the accounts that have voted for that poll.

### Transaction endpoints:

All transaction endpoints come in pairs, `/api/:endpoint/formTransaction`,
which takes some arguments, and an address of the user signing the transaction,
and returns a transaction that represents the desired action, and a second
endpoint `/api/:endpoint/postTransaction`, which take the same arguments, the
address, and the signed version of the transaction, and posts the transaction
to the chain through the backend, and tracks the mining status of the
transaction, and its effect once it is mined. In `/aev_util.js` you can see the
function `form_and_post` which takes a path such as "/api/vote" or
"/api/revokeVote", and an object representing all of the arguments associated
with the transaction, and performs both steps of this process, using sidekick
to sign the transaction in between.

```
POST /api/vote/formTransaction

request: {
    poll_id: number,
    option_id: number,

    address: string
}

response: {
    tx: string
}
```

```
POST /api/vote/postTransaction

request: {
    poll_id: number,
    option_id: number,

    address: string,
    signed_tx: string
}

response: {
}
```

Endpoints for a user to vote on a particular option in a particular poll.

```
POST /api/revokeVote/formTransaction

request: {
    poll_id: number,

    address: string
}

response: {
    tx: string
}
```

```
POST /api/revokeVote/postTransaction

request: {
    poll_id: number,

    address: string,
    signed_tx: string
}

response: {
}
```

Endpoints to revoke a vote that has been made.

```
POST /api/createPoll/formTransaction

request: {
    title: string,
    description: string,
    url: string,
    lifetime: "never_closes" | string,
    options: string[],

    address: string
}

response: {
    tx: string
}
```

```
POST /api/createPoll/postTransaction

request: {
    title: string,
    description: string,
    url: string,
    lifetime: "never_closes" | string,
    options: string[],

    address: string,
    signed_tx: string
}

response: {
    tx_hash: string
}
```

Endpoints to create a new poll. The "address" field must be a user who has
permission to create polls, according to the admin endpoints.

### Admin endpoints:

Similar to transaction endpoints, admin endpoints come in pairs. One
`/api/:endpoint/formMessage` to build a message describing the specific action
that the user asked for, that the user can sign to prove to the backend that
they really are the holder of the account they are trying to perform the action
as. This allows us to filter who can do certain actions based on which account
they are trying to sign these messages with. Ironically, half of the admin
endpoints are related to managing exactly these permissions, but the other half
actually do things related to what end users see on the main site.

Testing these endpoints might require having an account that can use them in
the first place. If you are running an endpoint then you can edit the file
named `permissions`, to add an association `"ak_yourAddress" => 3` to give it
all of the permissions. Signing messages doesn't post anything to the
blockchain, so you can use whatever addresses you want, without even having a
node running, and without leaving a permanent trail on the testnet of who your
addresses belong to, or what voting app development they have been associated
with.

```
POST /api/getSettings/formMessage

request: {
    address: string
}

response: {
    timestamp: number,
    nonce: number,
    message: string
}
```

```
POST /api/getSettings/submitSig

request: {
    address: string,
    timestamp: number,
    nonce: number,
    message_signature: string
}

response: {
    poll_categories: {
        [poll_address: string]: "default" | "hidden" | "all" | "approved" | "official"
    },
    account_categories: {
        [account_address: string]: "hidden" | "all" | "approved" | "official"
    },
    account_permissions: {
        [account_address: string]: "none" | "can_create_polls" | "can_set_categories" | "can_change_permissions"
    }
}
```

Only users with permissions to set categories or to set account permissions
can see the list of categories and settings. Once they sign a message, three
lists can be retrieved, and built into tables of polls. The
`account_categories` and `account_permissions` lists might have different
accounts listed in them, so if you want to combine them into a single table,
some extra logic has to be applied, which can be seen in the function
`generateAuthorTable`, in `/polls/admin` in the example site. Also, accounts
that haven't ever created polls might need to gain the right to create them, so
`/polls/admin` also demonstrates a simple HTML form that can be used to add
users who aren't in the table already.

```
POST /api/setPollCategory/formMessage

request: {
    poll_id: number,
    category: "default" | "hidden" | "all" | "approved" | "official",

    address: string
}

response: {
    timestamp: number,
    nonce: number,
    message: string
}
```

```
POST /api/setPollCategory/submitSig

request: {
    poll_id: number,
    category: "default" | "hidden" | "all" | "approved" | "official",

    address: string,
    timestamp: number,
    nonce: number,
    message_signature: string
}

response: {
}
```

Endpoints for setting the category override of a poll. The rules for working
out what category a poll belongs to are as follows:
1. If the poll has a setting other than "default", then it is always displayed
   with that category.
2. Otherwise, if this poll index is set to default, and if the same contract
   has been registered with a smaller index than this one, this one will be
   hidden.
3. Otherwise, if this poll is set to default, and is the lowest index
   associated with this contract address, and the account that created the
   contract is set to a particular category, then it will display as that
   blanket setting.
4. Finally, if none of the above rules apply, it is set to "all".

Step 2 of this logic is irrelevant now that polls are created and registered in
one step, but is very relevant if we want to use the backend to display
historical polls as well.

```
POST /api/setAccountCategory/formMessage

request: {
    account: string,
    category: "hidden" | "all" | "approved" | "official",

    address: string
}

response: {
    timestamp: number,
    nonce: number,
    message: string
}
```

```
POST /api/setAccountCategory/submitSig

request: {
    account: string,
    category: "hidden" | "all" | "approved" | "official",

    address: string,
    timestamp: number,
    nonce: number,
    message_signature: string
}

response: {
}
```

Set the blanket category of all polls created by a given user.

```
POST /api/setAccountPermissions/formMessage

request: {
    account: string,
    permission_level: "none" | "can_create_polls" | "can_set_categories" | "can_change_permissions",

    address: string
}

response: {
    timestamp: number,
    nonce: number,
    message: string
}
```

```
POST /api/setAccountPermissions/submitSig

request: {
    account: string,
    permission_level: "none" | "can_create_polls" | "can_set_categories" | "can_change_permissions",

    address: string,
    timestamp: number,
    nonce: number,
    message_signature: string
}

response: {
}
```

Set the permission level that a user has, to use the above admin endpoints, and
to create polls. Similar to poll categories, these settings are inclusive,
anyone who can set categories can create polls, and anyone who can set
permissions can do both. (Since they would have been able to give themselves
those permissions anyway.)

