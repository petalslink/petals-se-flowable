/**
 * Copyright (c) 2019-2023 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_EXECUTIONS_SERVICE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETEXECUTIONS;

import org.junit.Test;
import org.ow2.petals.components.flowable.generic._1.GetExecutions;
import org.ow2.petals.components.flowable.generic._1.GetExecutionsResponse;
import org.ow2.petals.flowable.incoming.integration.GetExecutionsOperation;

/**
 * Unit tests about request processing of the service {@link GetExecutionsOperation}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetExecutionsInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link GetExecutionsOperation} when:
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
    public void invalidRequest_WsdlUncompliant() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_EXECUTIONS_SVC_CFG, ITG_EXECUTIONS_PORT_TYPE,
                ITG_EXECUTIONS_SERVICE, ITG_OP_GETEXECUTIONS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetExecutionsOperation} when:
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
    public void invalidRequest_WsdlCompliant() throws Exception {

        // We use a response as request
        this.testInvalidRequest_WsdlCompliant(NATIVE_EXECUTIONS_SVC_CFG, ITG_EXECUTIONS_PORT_TYPE,
                ITG_EXECUTIONS_SERVICE, ITG_OP_GETEXECUTIONS, new GetExecutionsResponse());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetExecutionsOperation} when:
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

        this.testInvalidRequest_Empty(NATIVE_EXECUTIONS_SVC_CFG, ITG_EXECUTIONS_PORT_TYPE, ITG_EXECUTIONS_SERVICE,
                ITG_OP_GETEXECUTIONS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetExecutionsOperation} when:
     * </p>
     * <ul>
     * <li>no argument is given</li>
     * </ul>
     * <p>
     * Expected results:
     * </p>
     * <ul>
     * <li>no error occurs</li>
     * <li>no fault occurs</li>
     * <li>no task returns because no process instance exists</li>
     * </ul>
     */
    @Test
    public void noArguments() throws Exception {

        final Object getExecutionsRespObj = this.testRequest(NATIVE_EXECUTIONS_SVC_CFG, ITG_EXECUTIONS_PORT_TYPE,
                ITG_EXECUTIONS_SERVICE, ITG_OP_GETEXECUTIONS,
                new GetExecutions());

        assertTrue(getExecutionsRespObj instanceof GetExecutionsResponse);
        final GetExecutionsResponse getExecutionsResp = (GetExecutionsResponse) getExecutionsRespObj;
        assertNotNull(getExecutionsResp.getExecutions());
        assertNotNull(getExecutionsResp.getExecutions().getExecution());
        assertEquals(0, getExecutionsResp.getExecutions().getExecution().size());
    }

}
