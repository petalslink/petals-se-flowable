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
package org.ow2.petals.flowable.rest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.File;
import java.net.URL;

import javax.xml.namespace.QName;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.ow2.petals.component.framework.junit.impl.ProvidesServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.rule.ParameterGenerator;
import org.ow2.petals.component.framework.junit.rule.ServiceConfigurationFactory;
import org.ow2.petals.flowable.FlowableSEConstants;

import com.fasterxml.jackson.databind.JsonNode;

import jakarta.ws.rs.core.Response;
import jakarta.ws.rs.core.Response.Status;

/**
 * Class for unit tests about the use of the REST API with the default privilege configuration
 * 
 * @author Victor NOEL - Linagora
 * @author Jordy CABANNES - Linagora
 * @author Christophe DENEUX - Linagora
 */
public class RestApiDefaultPrivConfigTest extends AbstractRestTestEnvironment {

    protected static final String INONLY_SU = "in-only";

    protected static final String INONLY_SU_HOME = "su/in-only/";

    private static final String INONLY_NAMESPACE = "http://petals.ow2.org/se-flowable/unit-test/in-only";

    protected static final QName INONLY_INTERFACE = new QName(INONLY_NAMESPACE, "in-only");

    protected static final QName INONLY_SERVICE = new QName(INONLY_NAMESPACE, "in-only-service");

    protected static final String INONLY_ENDPOINT = "edpInOnly";

    @BeforeAll
    private static void completesComponentUnderTestConfiguration() throws Exception {
        COMPONENT_UNDER_TEST
                .setParameter(new QName(FlowableSEConstants.NAMESPACE_COMP, FlowableSEConstants.ENGINE_REST_API_ENABLE),
                        Boolean.TRUE.toString())
                .setParameter(
                        new QName(FlowableSEConstants.NAMESPACE_COMP,
                                FlowableSEConstants.IDM_ENGINE_CONFIGURATOR_CFG_FILE),
                        // Generate identity service configuration files
                        new ParameterGenerator() {

                            @Override
                            public String generate() throws Exception {
                                final URL idmEngineConfiguratorCfg = Thread.currentThread().getContextClassLoader()
                                        .getResource("rest/idm-engine-configurator.properties");
                                assertNotNull(idmEngineConfiguratorCfg,
                                        "IDM engine configurator config file is missing !");
                                return new File(idmEngineConfiguratorCfg.toURI()).getAbsolutePath();
                            }

                        })
                .registerServiceToDeploy(INONLY_SU, new ServiceConfigurationFactory() {
                    @Override
                    public ServiceConfiguration create() {

                        final URL wsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(INONLY_SU_HOME + "in-only.wsdl");
                        assertNotNull(wsdlUrl, "WSDL not found");
                        final ProvidesServiceConfiguration serviceConfiguration = new ProvidesServiceConfiguration(
                                INONLY_INTERFACE, INONLY_SERVICE, INONLY_ENDPOINT, wsdlUrl);

                        final URL startResponseXslUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(INONLY_SU_HOME + "startResponse.xsl");
                        assertNotNull(startResponseXslUrl, "Output XSL 'startResponse.xsl' not found");
                        serviceConfiguration.addResource(startResponseXslUrl);

                        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(INONLY_SU_HOME + "in-only.bpmn");
                        assertNotNull(bpmnUrl, "BPMN file not found");
                        serviceConfiguration.addResource(bpmnUrl);

                        final URL archivageServiceWsdlUrl = Thread.currentThread().getContextClassLoader()
                                .getResource(INONLY_SU_HOME + "archivageService.wsdl");
                        assertNotNull(archivageServiceWsdlUrl, "archivageService WSDL not found");
                        serviceConfiguration.addResource(archivageServiceWsdlUrl);

                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "process_file"), "in-only.bpmn");
                        serviceConfiguration.setServicesSectionParameter(
                                new QName(FlowableSEConstants.NAMESPACE_SU, "version"), "1");

                        return serviceConfiguration;
                    }
                }).postInitComponentUnderTest();
    }

    /**
     * Check that we cannot access to REST API without login/password.
     */
    @Test
    public void getOnApiNoLogin() {
        final Response response = deployments(null, null).get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with a bad login/password.
     */
    @Test
    public void getOnApiBadLogin() {
        final Response response = deployments("rest-api-user", "lololol").get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we cannot access to REST API with login/password of a user who does not belong to the
     * ENGINE_REST_API_ACCESS_PRIVILEGE.
     */
    @Test
    public void getOnApiIncorrectLogin() {
        final Response response = deployments("fozzie", "fozzie").get();

        assertEquals(Status.UNAUTHORIZED.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we can access to REST API with login/password of a user who belongs to the
     * ENGINE_REST_API_ACCESS_PRIVILEGE.
     */
    @Test
    public void getOnApiCorrectLogin() {
        final Response response = deployments("rest-api-user", "user-api-rest-password").get();

        assertEquals(Status.OK.getStatusCode(), response.getStatus());
    }

    /**
     * Check that we retrieve correct data through the REST API. Expected result: the process that was instantiated in
     * this class
     */
    @Test
    public void hasDeployments() {
        final JsonNode response = deployments("rest-api-user", "user-api-rest-password").get(JsonNode.class);

        final JsonNode jsonNode = response.get("data");
        assertTrue(jsonNode.isArray());
        assertEquals(1, jsonNode.size());
        assertEquals("Process read from: in-only.bpmn", jsonNode.get(0).get("name").asText());
    }
}
