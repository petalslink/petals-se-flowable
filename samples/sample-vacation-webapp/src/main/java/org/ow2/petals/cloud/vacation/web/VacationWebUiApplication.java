/**
 * Copyright (c) 2015-2017 Linagora
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
package org.ow2.petals.cloud.vacation.web;

import javax.xml.datatype.DatatypeConfigurationException;
import javax.xml.datatype.DatatypeFactory;

import org.ow2.petals.cloud.vacation.web.VacationRequest.PendingVacationRequest;
import org.ow2.petals.cloud.vacation.web.services.ActivitiProcessClient;
import org.ow2.petals.cloud.vacation.web.services.ActivitiTaskClient;
import org.ow2.petals.cloud.vacation.web.services.VacationClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.boot.context.web.SpringBootServletInitializer;
import org.springframework.context.annotation.Bean;
import org.springframework.core.convert.converter.Converter;
import org.springframework.oxm.jaxb.Jaxb2Marshaller;

/**
 * 
 * @author vnoel
 *
 */
@SpringBootApplication
public class VacationWebUiApplication extends SpringBootServletInitializer {

    @Bean
    public Converter<String, PendingVacationRequest> requestConverter(final ActivitiProcessClient processClient) {
        return new Converter<String, PendingVacationRequest>() {
            @Override
            public PendingVacationRequest convert(final String id) {
                return processClient.getPendingRequest(id);
            }
        };
    }

    @Bean
    public DatatypeFactory dtf() throws DatatypeConfigurationException {
        return DatatypeFactory.newInstance();
    }

    @Bean
    public Jaxb2Marshaller marshaller() {
        final Jaxb2Marshaller marshaller = new Jaxb2Marshaller();
        marshaller.setContextPaths("org.ow2.petals.components.activiti.generic._1",
                "org.ow2.petals.samples.se_bpmn.vacationrequest", "org.ow2.petals.samples.se_bpmn.vacationservice");
        return marshaller;
    }

    @Bean
    public ActivitiProcessClient activitiProcessClient(
            final @Value("${activiti.service.process.url}") String activitiProcessURL,
            final Jaxb2Marshaller marshaller) {
        final ActivitiProcessClient client = new ActivitiProcessClient();
        client.setDefaultUri(activitiProcessURL);
        client.setMarshaller(marshaller);
        client.setUnmarshaller(marshaller);
        return client;
    }

    @Bean
    public ActivitiTaskClient activitiTaskClient(final @Value("${activiti.service.task.url}") String activitiTaskURL,
            final Jaxb2Marshaller marshaller) {
        final ActivitiTaskClient client = new ActivitiTaskClient();
        client.setDefaultUri(activitiTaskURL);
        client.setMarshaller(marshaller);
        client.setUnmarshaller(marshaller);
        return client;
    }

    @Bean
    public VacationClient vacationClient(final @Value("${vacation.service.url}") String vacationURL,
            final Jaxb2Marshaller marshaller) {
        final VacationClient client = new VacationClient();
        client.setDefaultUri(vacationURL);
        // Normally it's ok to refer to a bean like that...
        client.setMarshaller(marshaller);
        client.setUnmarshaller(marshaller);
        return client;
    }


    /**
     * This is for the real servlet container
     */
    @Override
    protected SpringApplicationBuilder configure(final SpringApplicationBuilder builder) {
        return builder.sources(VacationWebUiApplication.class);
    }

    /**
     * This is for test with spring-boot
     */
    public static void main(final String[] args) throws Exception {
		SpringApplication.run(VacationWebUiApplication.class, args);
	}


}
