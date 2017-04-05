/**
 * Copyright (c) 2014-2017 Linagora
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

import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_OP_GETTASKS;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_PORT_TYPE;
import static org.ow2.petals.flowable.FlowableSEConstants.IntegrationOperation.ITG_TASK_SERVICE;

import org.junit.Test;
import org.ow2.petals.components.flowable.generic._1.GetTasks;
import org.ow2.petals.components.flowable.generic._1.GetTasksResponse;
import org.ow2.petals.flowable.incoming.integration.GetTasksOperation;

/**
 * Unit tests about request processing of the service {@link GetTasksOperation}
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class GetTasksInvocationTest extends AbstractIntegrationServiceInvokations {

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is not compliant to the XML schema defined in WSDL</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void invalidRequest_WsdlUncompliantRequest() throws Exception {

        this.testInvalidRequest_WsdlUncompliant(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                ITG_OP_GETTASKS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an invalid request is sent,</li>
     * <li>the request content is compliant to the XML schema defined in WSDL</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void invalidRequest_WsdlCompliantRequest() throws Exception {

        // We use a response as request
        this.testInvalidRequest_WsdlCompliant(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE,
                ITG_OP_GETTASKS,
                new GetTasksResponse());
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>an empty request is sent</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>a fault occurs about the invalid request</li>
     * </ul>
     * </p>
     */
    @Test
    public void invalidRequest_EmptyRequest() throws Exception {

        this.testInvalidRequest_Empty(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE, ITG_TASK_SERVICE, ITG_OP_GETTASKS);
    }

    /**
     * <p>
     * Check the processing of the integration service {@link GetTasksOperation} when:
     * <ul>
     * <li>no argument is given</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>no error occurs</li>
     * <li>no fault occurs</li>
     * <li>no task returns because no process instance exists</li>
     * </ul>
     * </p>
     */
    @Test
    public void noArguments() throws Exception {

        final Object getTasksRespObj = this.testValidRequest_NoArguments(NATIVE_TASKS_SVC_CFG, ITG_TASK_PORT_TYPE,
                ITG_TASK_SERVICE,
                ITG_OP_GETTASKS, new GetTasks());

        assertTrue(getTasksRespObj instanceof GetTasksResponse);
        final GetTasksResponse getTasksResp = (GetTasksResponse) getTasksRespObj;
        assertNotNull(getTasksResp.getTasks());
        assertNotNull(getTasksResp.getTasks().getTask());
        assertEquals(0, getTasksResp.getTasks().getTask().size());
    }

}
