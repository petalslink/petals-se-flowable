/**
 * Copyright (c) 2017-2026 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.rest;

import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.net.ConnectException;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.ow2.petals.flowable.FlowableSEConstants;

import jakarta.ws.rs.ProcessingException;

/**
 * Class for unit tests about the REST API activation
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
public class RestDisabledTest extends AbstractRestTestEnvironment {

    @BeforeAll
    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_ENABLE),
                        Boolean.FALSE.toString())
                .postInitComponentUnderTest();
    }

    /**
     * Check that we cannot access to the REST API if it is not enabled and that the server is not even started.
     */
    @Test
    public void getOnApiDisabled() {

        final ProcessingException actualException = assertThrows(ProcessingException.class, () -> {
            deployments("rest-api-user", "user-api-rest-password").get();
        });
        assertInstanceOf(ConnectException.class, actualException.getCause());
    }
}
