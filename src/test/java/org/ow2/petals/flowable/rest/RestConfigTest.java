/**
 * Copyright (c) 2017 Linagora
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

import java.net.ConnectException;

import javax.ws.rs.ProcessingException;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;
import javax.xml.namespace.QName;

import org.hamcrest.core.IsInstanceOf;
import org.junit.ClassRule;
import org.junit.Test;
import org.junit.rules.RuleChain;
import org.junit.rules.TestRule;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTest;
import org.ow2.petals.flowable.FlowableSEConstants;

/**
 * Class for unit tests about the REST API config
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 */
public class RestConfigTest extends AbstractRestTestEnvironment {

    private static final int CUSTOMIZED_PORT = 8085;

    protected static final ComponentUnderTest COMPONENT_UNDER_TEST = new ComponentUnderTest()
            .addLogHandler(IN_MEMORY_LOG_HANDLER.getHandler())
            .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_PORT),
                    String.valueOf(CUSTOMIZED_PORT))
            .setParameter(
                    new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_ACCESS_GROUP),
                    "user");

    @ClassRule
    public static final TestRule chain = RuleChain.outerRule(TEMP_FOLDER).around(IN_MEMORY_LOG_HANDLER)
            .around(COMPONENT_UNDER_TEST);

    @Override
    protected ComponentUnderTest getComponentUnderTest() {
        return COMPONENT_UNDER_TEST;
    }

    /**
     * Check that we can access to the REST API with the new configuration (customized ENGINE_REST_API_PORT and
     * ENGINE_REST_API_ACCESS_GROUP).
     */
    @Test
    public void getOnApiCorrectConfig() {
        final Response response = deployments("fozzie", "fozzie", CUSTOMIZED_PORT).get();

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with login/password of a user who does not belong to the customized
     * ENGINE_REST_API_ACCESS_GROUP.
     */
    @Test
    public void getOnApiIncorrectLogin() {
        final Response response = deployments("rest-api-user", "user-api-rest-password", CUSTOMIZED_PORT).get();

        assertEquals(Status.FORBIDDEN.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to the REST API on the default port.
     */
    @Test
    public void getOnOriginalPortFails() {
        expected.expect(ProcessingException.class);
        expected.expectCause(IsInstanceOf.<ConnectException> instanceOf(ConnectException.class));

        deployments("rest-api-user", "user-api-rest-password").get();
    }
}
