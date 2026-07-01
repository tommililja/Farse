namespace Farse.Tests

open Farse

module Data =

    let example =
        JObj [
            "id", JStr "c8eae96a-025d-4bc9-88f8-f204e95f2883"
            "name", JStr "Alice"
            "age", JNil
            "email", JStr "alice@domain.com"
            "profiles",
                JArr [
                    JStr "01458283-b6e3-4ae7-ae54-a68eb587cdc0"
                    JStr "927eb20f-cd62-470c-aafc-c3ce6b9248b0"
                    JStr "bf00d1e2-ee53-4969-9507-86bed7e96432"
                ]
            "subscription",
                JObj [
                    "plan", JStr "pro"
                    "isCanceled", JBit false
                    "renewsAt", JStr "2026-12-25T10:30:00Z"
                ]
            "tags",
                JArr [
                    JStr "beta"
                    JStr "verified"
                ]
        ]