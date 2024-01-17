/**
 * Copyright (c) 2017-2024 Linagora
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
package org.ow2.petals.flowable.integration;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETUSER;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_USER_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_USER_SERVICE;

import org.junit.jupiter.api.Test;
import org.ow2.petals.components.flowable.generic._1.GetUser;
import org.ow2.petals.components.flowable.generic._1.GetUserResponse;
import org.ow2.petals.components.flowable.generic._1.UnknownUser;
import org.ow2.petals.components.flowable.generic._1.User;
import org.ow2.petals.flowable.incoming.integration.GetUserOperation;

/**
 * Unit tests about request processing of the service {@link GetUserOperation}
 * 
 * @author Christophe DENEUX - Linagora
 */
public class GetUserInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link GetUserOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlUncompliantRequest() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_USER_SVC_CFG, ITG_USER_PORT_TYPE, ITG_USER_SERVICE,
                ITG_OP_GETUSER);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetUserOperation} when:
     * </p>
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_WsdlCompliantRequest() throws Exception {

        // We use a response as request
        final GetUserResponse request = new GetUserResponse();
        request.setUser(new User());
        this.testInvalidRequest_WsdlCompliant(NATIVE_USER_SVC_CFG, ITG_USER_PORT_TYPE, ITG_USER_SERVICE, ITG_OP_GETUSER,
                request);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetUserOperation} when:
     * </p>
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     */
    @Test
    public void invalidRequest_EmptyRequest() throws Exception {

        this.testInvalidRequest_Empty(NATIVE_USER_SVC_CFG, ITG_USER_PORT_TYPE, ITG_USER_SERVICE, ITG_OP_GETUSER);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetUserOperation} when:
     * </p>
     * <ul>
     * <li>valid arguments are given to retrieve a user</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>user information are correctly retrieved</li>
     * </ul>
     */
    @Test
    public void validRequest() throws Exception {

        final String userId = BPMN_USER_DEMANDEUR;
        final GetUser bean = new GetUser();
        bean.setId(userId);
        final Object getUserRespObj = this.testRequest(NATIVE_USER_SVC_CFG, ITG_USER_PORT_TYPE, ITG_USER_SERVICE,
                ITG_OP_GETUSER, bean);

        assertInstanceOf(GetUserResponse.class, getUserRespObj);
        assertNotNull(((GetUserResponse) getUserRespObj).getUser());
        assertEquals(userId, ((GetUserResponse) getUserRespObj).getUser().getId());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetUserOperation} when:
     * </p>
     * <ul>
     * <li>valid arguments are given to retrieve an unknown user</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>fault 'Unknown User' is returned</li>
     * </ul>
     */
    @Test
    public void unkownUser() throws Exception {

        final String userId = "unknown-user";
        final GetUser bean = new GetUser();
        bean.setId(userId);
        final Object getUserRespObj = this.testRequestWithFault(NATIVE_USER_SVC_CFG, ITG_USER_PORT_TYPE,
                ITG_USER_SERVICE, ITG_OP_GETUSER, bean);

        assertInstanceOf(UnknownUser.class, getUserRespObj);
        assertEquals(userId, ((UnknownUser) getUserRespObj).getId());
    }
}
