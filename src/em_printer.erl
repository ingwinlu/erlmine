%% -*- coding: utf-8 -*-
-module(em_printer).

-callback print(Field :: em_field:field(),
                Moves :: queue:queue(em_field:selection())) ->
                    iolist().
