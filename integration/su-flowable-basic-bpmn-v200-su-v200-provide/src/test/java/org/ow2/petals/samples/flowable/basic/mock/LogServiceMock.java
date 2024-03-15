/**
 * Copyright (c) 2020-2024 Linagora
 *
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the New BSD License (3-clause license).
 *
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the New BSD License (3-clause license)
 * for more details.
 *
 * You should have received a copy of the New BSD License (3-clause license)
 * along with this program/library; If not, see http://directory.fsf.org/wiki/License:BSD_3Clause/
 * for the New BSD License (3-clause license).
 */
package org.ow2.petals.samples.flowable.basic.mock;

import javax.xml.ws.WebServiceException;

import org.ow2.petals.integration.tests.se_flowable.log.services.v2.Log;

public class LogServiceMock implements Log {

    @Override
    public void log(final String level, final String handler, final String message) {

        if (level == null || level.trim().isEmpty() || handler == null || handler.trim().isEmpty() || message == null
                || message.trim().isEmpty()) {
            throw new WebServiceException(
                    String.format("Missing or empty parameter: level = %s, handler = %s, message = %s", level, handler,
                            message));
        }

        System.out.println(String.format("LOG Service: %s - %s : %s", level, handler, message));
    }

}
